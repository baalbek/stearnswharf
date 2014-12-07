{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module StearnsWharf.Output where

import Text.Printf (printf)

import Numeric.LinearAlgebra (Vector)
import Data.Packed.Vector ((@>),subVector) 

import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Profiles as P

data NodeResult = NodeResult {
                    nrId :: N.NodeId,
                    forces :: Vector Double,
                    displacements :: Vector Double,
                    stresses :: M.Stress 
                } 
                deriving Show

data BeamResult = BeamResult { brId :: B.BeamId, 
                               desc :: String,
                               nr1 :: NodeResult,
                               nr2 :: NodeResult } deriving Show

shear :: NodeResult -> Double
shear nr = (forces nr) @> 1
    
normalf :: NodeResult -> Double
normalf nr = (forces nr) @> 0

moment :: NodeResult -> Double
moment nr = (forces nr) @> 2

yTrans :: NodeResult -> Double
yTrans nr = (displacements nr) @> 1

xTrans :: NodeResult -> Double
xTrans nr = (displacements nr) @> 0

collectResult :: P.Profile a => Vector Double -> Vector Double -> B.Beam a -> BeamResult
collectResult vForces vDeflect beam = BeamResult (B.beamId beam) (P.desc $ B.bt beam) nr1 nr2
    where locV = B.localDisps beam vDeflect 
          locF = B.localForces beam $ B.localDisps beam vForces
          nodeV i = (subVector i 3 locV) 
          nodeF i = (subVector i 3 locF) 
          (stress1,stress2) = B.localStresses beam locF
          nr1 = NodeResult (N.nodeId (B.n1 beam)) (nodeF 0) (nodeV 0) stress1
          nr2 = NodeResult (N.nodeId (B.n2 beam)) (nodeF 3) (nodeV 3) stress2

collectNodes :: [BeamResult] -> [NodeResult]
collectNodes [] = []
collectNodes [x] = (nr1 x) : (nr2 x) : []
collectNodes (x:xs) = (nr1 x) : (nr2 x) : collectNodes xs

maxProperty :: (NodeResult -> Double) -> [NodeResult] -> NodeResult
maxProperty _ [] = error "maxProperty : empty list"
maxProperty _ [x] = x
maxProperty maxFun (x:xs) 
    | (abs (maxFun x)) > (abs (maxFun maxTail)) = x
    | otherwise = maxTail
    where maxTail = maxProperty maxFun xs 

printNodeResults :: NodeResult -> IO ()
printNodeResults nr = do
    printf "\tNode: [%s]\n" $ nrId nr
    printf "\t\tNormal: %.2f kN\n" $ normalf nr
    printf "\t\tShear: %.2f kN\n" $ shear nr 
    printf "\t\tMoment: %.2f kNm\n" $ moment nr
    printf "\t\tLocal displacement [x]: %.2f mm\n" $ (xTrans nr) * 1000.0
    printf "\t\tLocal displacement [y]: %.2f mm\n" $ (yTrans nr) * 1000.0
    printf "\t\tSigma : %.2f N/mm2\n" $ M.sigma $ stresses nr
    printf "\t\tTau : %.2f N/mm2\n" $ M.tau $ stresses nr


printResults :: BeamResult -> IO ()
printResults BeamResult { brId,desc,nr1,nr2 } = do 
    printf "Beam: %s [%s]\n" desc brId
    printNodeResults $ nr1
    printNodeResults $ nr2

printSummary :: [BeamResult] -> IO ()
printSummary brs = do
    putStrLn "******************* Summary ************************"
    let nodes = collectNodes brs
    let maxM = maxProperty moment nodes
    let maxS = maxProperty shear nodes
    let maxV = maxProperty yTrans nodes
    printf "Max moment [%s]: %.2f kNm\n" (nrId maxM) (moment maxM)
    printf "Max shear [%s]: %.2f kN\n" (nrId maxS) (shear maxS)
    printf "Max deflection [%s]: %.2f mm\n" (nrId maxV) (1000.0 * (yTrans maxV))

