{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module StearnsWharf.System where

import qualified Text.XML.Light as XML

import Numeric.LinearAlgebra ( (#>),Matrix,Vector,inv )
import Numeric.LinearAlgebra.Devel ( runSTMatrix,runSTVector,newVector,newMatrix )

import Data.Map (elems)
import Database.PostgreSQL.Simple (close)

import StearnsWharf.Common (getConnection)
import qualified StearnsWharf.CmdLine as X
import qualified StearnsWharf.Repos.NodeRepository as NR
import qualified StearnsWharf.Repos.LoadRepository as LR
import qualified StearnsWharf.Repos.SteelElementRepository as SR
import qualified StearnsWharf.Repos.WoodElementRepository as WR
import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Output as OUT

import qualified StearnsWharf.XML.XmlNodes as XN
import qualified StearnsWharf.XML.XmlLoads as XL
import qualified StearnsWharf.XML.XmlProfiles as XP

import qualified StearnsWharf.Wood.WoodProfiles as WP
import qualified StearnsWharf.Steel.SteelProfiles as SP

data ProfileContext = ProfileContext {
                            steelProfiles :: [B.Beam SP.SteelProfile],
                            woodProfiles :: [B.Beam WP.WoodProfile],
                            pointLoads :: [L.PointLoad],
                            numDof :: Int
                        }
                        deriving Show

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
          resultForces = invSysK #> (sysSyWo + sysPwo)
          resultDeflections = invSysK #> (sysSyWith + sysPwith)

beamResults :: ProfileContext -> Vector Double -> Vector Double -> [OUT.BeamResult]
beamResults ProfileContext { steelProfiles,woodProfiles } vUltimateLimit vServicabilityLimit = concat [woodResults,steelResults]
    where woodResults | null woodProfiles == True = []
                      | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) woodProfiles
          steelResults | null steelProfiles == True = []
                       | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) steelProfiles

getProfileContext :: String    -- ^ Database Host
                     -> String -- ^ Database Name
                     -> String -- ^ Database User
                     -> Int    -- ^ System Id
                     -> Int    -- ^ Load Case
                     -> IO ProfileContext
getProfileContext host dbname user sysId loadCase =
    getConnection host dbname user >>= \c ->
    NR.fetchNodesAsMap c sysId >>= \nx ->
    LR.fetchDistLoadsAsMap c sysId >>= \lx ->
    LR.systemPointLoads c sysId nx >>= \px ->
    SR.systemSteelElements c sysId loadCase nx lx >>= \steels ->
    WR.systemWoodElements c sysId nx lx >>= \woods ->
    let numDof = systemDof (elems nx)
        ctx = ProfileContext steels woods px numDof in
    close c >>
    return ctx

runStearnsWharf :: X.CmdLine -> IO ()
runStearnsWharf x =
    getProfileContext (X.host x) (X.dbname x) (X.user x) (X.system x) (X.loadcase x) >>= \ctx ->
    let (rf,rd) = calcDeflections ctx
        result = beamResults ctx rf rd in
    mapM_ OUT.printResults result >>
    OUT.printNodeLoads (pointLoads ctx) >>
    OUT.printSummary result >>
    return ()

stearnsWharfResultXML :: XML.Element -> [OUT.BeamResult]
stearnsWharfResultXML doc = []
{-
stearnsWharfResultXML doc = beamResults ctx rf rd
    where loads = XL.createLoads doc
          nodes = XN.createMatstatNodes doc
          numDof = systemDof (elems nodes)
          ploads = XL.createPointLoads nodes doc
          steels = runReader (XP.createSteelProfiles doc) (nodes,loads)
          woods = runReader (XP.createWoodProfiles doc) (nodes,loads)
          ctx = ProfileContext steels woods ploads numDof
          (rf,rd) = calcDeflections ctx
-}

runStearnsWharfXML :: X.CmdLine -> IO ()
runStearnsWharfXML x =
    let Just xmlFile = X.xmlfile x in
    putStrLn xmlFile >>
    readFile xmlFile >>= \s ->
    case XML.parseXMLDoc s of
        Nothing -> error "Failed to parse xml"
        Just doc -> putStrLn "runStearnsWharf doc"
