{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module StearnsWharf.Output where

import Text.Printf (printf)

import Control.Monad (mplus)

-- import Numeric.LinearAlgebra (Vector)
-- import Data.Packed.Vector ((@>),subVector) 

import Numeric.LinearAlgebra ( Vector,subVector )
import Numeric.LinearAlgebra.Devel ( at' )


import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Profiles as P

data NodeResult = NodeResult {
                    nrId :: N.NodeId,
                    nrDesc :: String,
                    forces :: Vector Double,
                    displacements :: Vector Double,
                    stresses :: M.Stress
                } 
                deriving Show

data BeamResult = 
    BeamResult {
        brId :: B.BeamId, 
        desc :: String,
        nodeSpan :: Double,
        nr1 :: NodeResult,
        nr2 :: NodeResult,
        load :: Maybe L.Load
    } deriving Show

shear :: NodeResult -> Double
shear nr = at' (forces nr) 1
    
normalf :: NodeResult -> Double
normalf nr = at' (forces nr) 0

moment :: NodeResult -> Double
moment nr = at' (forces nr) 2

yTrans :: NodeResult -> Double
yTrans nr = at' (displacements nr) 1

xTrans :: NodeResult -> Double
xTrans nr = at' (displacements nr) 0

collectResult :: P.Profile a => Vector Double -> Vector Double -> B.Beam a -> BeamResult
collectResult vForces vDeflect beam = BeamResult (B.beamId beam) (P.desc $ B.bt beam) (N.len geom) nr1 nr2 (B.ld beam)
    where locV = B.localDisps beam vDeflect 
          locF = B.localForces beam $ B.localDisps beam vForces
          nodeV i = (subVector i 3 locV) 
          nodeF i = (subVector i 3 locF) 
          (stress1,stress2) = B.localStresses beam locF
          curN1 = B.n1 beam
          curN2 = B.n2 beam
          geom = N.calcGeom curN2 curN1
          nr1 = NodeResult (N.nodeId curN1) (N.desc curN1) (nodeF 0) (nodeV 0) stress1
          nr2 = NodeResult (N.nodeId curN2) (N.desc curN2) (nodeF 3) (nodeV 3) stress2
            

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
    printf "\tNode %s: [%d]\n" (nrDesc nr) (nrId nr)
    printf "\t\tNormal: %.2f kN\n" $ normalf nr
    printf "\t\tShear: %.2f kN\n" $ shear nr 
    printf "\t\tMoment: %.2f kNm\n" $ moment nr
    printf "\t\tLocal displacement [x]: %.2f mm\n" $ (xTrans nr) * 1000.0
    printf "\t\tLocal displacement [y]: %.2f mm\n" $ (yTrans nr) * 1000.0
    printf "\t\tSigma : %.2f N/mm2\n" $ M.sigma $ stresses nr
    printf "\t\tTau : %.2f N/mm2\n" $ M.tau $ stresses nr

maybeLoadToString :: Maybe L.Load -> String
maybeLoadToString ld = ldStr 
    where ldStr = case ld of 
                    Just ld' -> show ld' 
                    Nothing -> "Load: -"

printResults :: BeamResult -> IO ()
printResults BeamResult { brId,desc,nodeSpan,nr1,nr2,load } = do 
    printf "Beam: %s [%d], span: %.2f m\n%s\n" desc brId nodeSpan (maybeLoadToString load) 
    printNodeResults $ nr1
    printNodeResults $ nr2

printSummary :: [BeamResult] -> IO ()
printSummary brs = do
    putStrLn "\n******************* Summary ************************"
    let nodes = collectNodes brs
    let maxM = maxProperty moment nodes
    let maxS = maxProperty shear nodes
    let maxV = maxProperty yTrans nodes
    printf "Max moment [%d]: %.2f kNm\n" (nrId maxM) (moment maxM)
    printf "Max shear [%d]: %.2f kN\n" (nrId maxS) (shear maxS)
    printf "Max deflection [%d]: %.2f mm\n\n" (nrId maxV) (1000.0 * (yTrans maxV))

{-
    PointLoad { ploadId :: LoadId, 
        plVal :: Double,
        node :: Node,
        plAngle :: Double,
        plFactor :: Double }
 -}

printNodeLoad :: L.PointLoad -> IO ()
printNodeLoad L.PointLoad { ploadId,plVal,node,plAngle,plFactor } = 
    printf "Node load [%d] node: %s, value: %.1f\n" ploadId (N.desc node) plVal

printNodeLoads :: [L.PointLoad] -> IO ()
printNodeLoads plx = 
    putStrLn "\n******************* Node Loads ************************" >>
    mapM_ printNodeLoad plx
                            
