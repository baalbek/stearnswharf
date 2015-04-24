{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Loads where

import Text.Printf (printf)

import qualified Data.Map as Map
import Control.Monad.ST

import Data.Packed.ST (STVector,modifyVector)

import StearnsWharf.Nodes (Node,systemIndexX,systemIndexY)
import StearnsWharf.Common (radians)

type LoadId = Int

data LoadVariant = WoFact | WithFact deriving (Show,Eq)

data Load = 
    Load {  loadId :: LoadId, 
            qz1 :: Double,
            qz2 :: Double,
            qx1 :: Double,
            qx2 :: Double,
            loadFactor :: Double } 
    | MultiLoad {
        loads :: [Load]
    }
    -- deriving Show

instance Show Load where
    show Load { loadId,qz1,qz2,loadFactor } = printf "Load: [%d] qz1: %.2f, qz2: %.2f, lf: %.2f" loadId qz1 qz2 loadFactor


type LoadMap = Map.Map Int Load

data PointLoad = 
    PointLoad { ploadId :: LoadId, 
        plVal :: Double,
        node :: Node,
        plAngle :: Double,
        plFactor :: Double }
    deriving Show

pointLoadForce :: (Double -> Double) -> LoadVariant -> PointLoad -> Double
pointLoadForce projFun loadVar PointLoad { plVal,plAngle,plFactor } = 
    case loadVar of WoFact -> result
                    WithFact -> result / plFactor
    where yproj = projFun $ radians plAngle
          result = plVal * yproj 

yForce :: LoadVariant -> PointLoad -> Double
yForce = pointLoadForce sin

xForce :: LoadVariant -> PointLoad -> Double
xForce = pointLoadForce cos

sysX :: PointLoad -> Maybe Int
sysX PointLoad { node } = systemIndexX node

sysY :: PointLoad -> Maybe Int
sysY PointLoad { node } = systemIndexY node

add2systemPointLoads :: STVector s Double -> LoadVariant -> PointLoad -> ST s ()
add2systemPointLoads vec loadVar load = do 
    case sysY load of Nothing -> return ()
                      Just yi  -> modifyVector vec yi (\x -> x + (yForce loadVar load)) 
    case sysX load of Nothing -> return ()
                      Just xi -> modifyVector vec xi (\x -> x + (xForce loadVar load)) 

cloneWithFactor :: Load -> Load
cloneWithFactor ld = Load (-1) qz1' qz2' qx1' qx2' lf 
    where qz1' = (qz1 ld) / lf
          qz2' = (qz2 ld) / lf
          qx1' = (qx1 ld) / lf
          qx2' = (qx2 ld) / lf
          lf   = loadFactor ld
        
