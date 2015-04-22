{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.System where

-- #define RCS_DEBUG

import Data.Map (elems)
import Numeric.LinearAlgebra (Matrix,Vector)
import Data.Packed.ST (newMatrix,runSTMatrix,newVector,runSTVector)
import Numeric.Container ((<>))
import Numeric.LinearAlgebra.Algorithms (inv)

import Database.PostgreSQL.Simple (close)

import StearnsWharf.Common (getConnection)

import qualified StearnsWharf.Repos.NodeRepository as NR
import qualified StearnsWharf.Repos.LoadRepository as LR
import qualified StearnsWharf.Repos.SteelElementRepository as SR
import qualified StearnsWharf.Repos.WoodElementRepository as WR
import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Output as OUT

import qualified StearnsWharf.Wood.WoodProfiles as WP
import qualified StearnsWharf.Steel.SteelProfiles as SP

data ProfileContext = ProfileContext {
                            steelProfiles :: [B.Beam SP.SteelProfile],
                            woodProfiles :: [B.Beam WP.WoodProfile],
                            pointLoads :: [L.PointLoad],
                            numDof :: Int
                        }

systemDof :: [N.Node] -> Int
systemDof nodes = (N.globNdx maxNode) + (N.numDof . N.dof) maxNode
    where maxNode = maximum nodes

systemSZ :: ProfileContext -> L.LoadVariant -> Vector Double
systemSZ ProfileContext { steelProfiles,woodProfiles,numDof } loadVar = runSTVector $ do
    v <- newVector 0.0 numDof 
    mapM_ (\x -> B.add2systemSZ v loadVar x) steelProfiles
    mapM_ (\x -> B.add2systemSZ v loadVar x) woodProfiles
    return v

systemPointLoads :: [L.PointLoad] -> Int -> L.LoadVariant -> Vector Double
systemPointLoads ploads numDof loadVar = runSTVector $ do
    v <- newVector 0.0 numDof 
    let myAdd = L.add2systemPointLoads v loadVar
    mapM_ myAdd ploads
    return v

systemK :: ProfileContext -> Matrix Double
systemK ProfileContext { steelProfiles,woodProfiles,numDof } = runSTMatrix $ do
    m <- newMatrix 0.0 numDof numDof 
    mapM_ (\x -> B.add2systemK m x) steelProfiles
    mapM_ (\x -> B.add2systemK m x) woodProfiles 
    return m

calcDeflections :: ProfileContext -> (Vector Double, Vector Double) 
calcDeflections ctx@ProfileContext { pointLoads,numDof } = (resultForces,resultDeflections)
    where invSysK = inv $ systemK ctx 
          sysSyWo = systemSZ ctx L.WoFact 
          sysSyWith = systemSZ ctx L.WithFact
          sysPwo = systemPointLoads pointLoads numDof L.WoFact 
          sysPwith = systemPointLoads pointLoads numDof L.WithFact 
          resultForces = invSysK <> (sysSyWo + sysPwo)
          resultDeflections = invSysK <> (sysSyWith + sysPwith)

beamResults :: ProfileContext -> Vector Double -> Vector Double -> [OUT.BeamResult]
beamResults ProfileContext { steelProfiles,woodProfiles } vUltimateLimit vServicabilityLimit = concat [woodResults,steelResults]
    where woodResults | null woodProfiles == True = []
                      | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) woodProfiles 
          steelResults | null steelProfiles == True = []
                       | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) steelProfiles 


runStearnsWharf :: String    -- ^ Database Host  
                   -> String -- ^ Database Name
                   -> String -- ^ Database User 
                   -> Int    -- ^ System Id
                   -> Int    -- ^ Load Case
                   -> IO ()
runStearnsWharf host dbname user sysId loadCase = 
    getConnection host dbname user >>= \c ->
    -- getConnection host "engineer" "engineer" >>= \c ->
    NR.fetchNodesAsMap c sysId >>= \nx -> 
    LR.fetchDistLoadsAsMap c sysId >>= \lx ->
    LR.systemPointLoads c sysId nx >>= \px ->
    SR.systemSteelElements c sysId loadCase nx lx >>= \steels ->
    WR.systemWoodElements c sysId nx lx >>= \woods ->
    let numDof = systemDof (elems nx)  
        ctx = ProfileContext steels woods px numDof 
        (rf,rd) = calcDeflections ctx 
        result = beamResults ctx rf rd in
    mapM_ OUT.printResults result >>
    OUT.printSummary result >> 
    close c >> 
    return ()

