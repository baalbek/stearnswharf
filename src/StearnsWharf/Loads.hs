module StearnsWharf.Loads where

import Control.Monad.ST

import Data.Packed.ST (STVector,modifyVector)

import StearnsWharf.Nodes (Node,systemIndexX,systemIndexY)
import StearnsWharf.Common (radians)

type LoadId = String

data LoadVariant = WoFact | WithFact deriving (Show,Eq)

data Load = 
    Load {  qy1 :: Double,
            qy2 :: Double,
            qx1 :: Double,
            qx2 :: Double,
            loadFactor :: Double } 
    | MultiLoad {
        loads :: [Load]
    }
            deriving Show

data PointLoad = PointLoad { 
                             plVal :: Double,
                             node :: Node,
                             plAngle :: Double,
                             plFactor :: Double }
            deriving Show

pointLoadForce :: (Double -> Double) -> LoadVariant -> PointLoad -> Double
pointLoadForce projFun loadVar (PointLoad a _ b f) = case loadVar of WoFact -> result
                                                                     WithFact -> result / f
    where yproj = projFun $ radians b   
          result = a * yproj 

yForce :: LoadVariant -> PointLoad -> Double
yForce = pointLoadForce sin

xForce :: LoadVariant -> PointLoad -> Double
xForce = pointLoadForce cos

sysX :: PointLoad -> Maybe Int
sysX (PointLoad _ n _ _) = systemIndexX n

sysY :: PointLoad -> Maybe Int
sysY (PointLoad _ n _ _) = systemIndexY n

add2systemPointLoads :: STVector s Double -> LoadVariant -> PointLoad -> ST s ()
add2systemPointLoads vec loadVar load = do 
    case sysY load of Nothing -> return ()
                      Just yi  -> modifyVector vec yi (\x -> x + (yForce loadVar load)) 
    case sysX load of Nothing -> return ()
                      Just xi -> modifyVector vec xi (\x -> x + (xForce loadVar load)) 

cloneWithFactor :: Load -> Load
cloneWithFactor ld = Load qy1' qy2' qx1' qx2' lf 
    where qy1' = (qy1 ld) / lf
          qy2' = (qy2 ld) / lf
          qx1' = (qx1 ld) / lf
          qx2' = (qx2 ld) / lf
          lf   = loadFactor ld
        
