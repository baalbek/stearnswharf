{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
module StearnsWharf.Beams where

-- #define RCS_DEBUG

#ifdef RCS_DEBUG
import Control.Monad.Writer (Writer,runWriter,tell)
import Text.Printf (printf)
#endif

import Data.Maybe (fromJust)
import Data.List (find)
import Numeric.LinearAlgebra (Matrix,Vector,fromList,fromLists)
--import Data.Packed.ST (STMatrix,writeMatrix,modifyMatrix,readMatrix,runSTMatrix,STVector,writeVector)
import Data.Packed.ST (STMatrix,modifyMatrix,STVector,modifyVector)
import Control.Monad.ST (ST)

--import Numeric.Container ((<>),vecdisp,dispf)
import Numeric.Container ((<>))
import Data.Packed.Matrix (trans,(@@>))
import Data.Packed.Vector ((@>),(|>)) 

import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Profiles as P

type BeamId = String

data Beam a = Bjlk33 { 
                beamId :: BeamId,
                n1,n2 :: N.Node,
                bt :: a,
                ld :: Maybe L.Load } 
            | Bjlk11 {
                beamId :: BeamId,
                n1,n2 :: N.Node,
                bt :: a,
                ld :: Maybe L.Load } 
            | Bjlk13 {
                beamId :: BeamId,
                n1,n2 :: N.Node,
                bt :: a,
                ld :: Maybe L.Load } 
            | Bjlk31 {
                beamId :: BeamId,
                n1,n2 :: N.Node,
                bt :: a,
                ld :: Maybe L.Load } 
            deriving Show

#ifdef RCS_DEBUG
createK :: P.Profile a => Beam a -> Writer String (Matrix Double)
createK (Bjlk33 _ n1' n2' bt' _) = do
    let beamK = fromLists [[eal, 0.0, 0.0, -eal, 0.0, 0.0],
                           [0.0, k11, k12, 0.0,  k14, k15],
                           [0.0, k12, k22, 0.0,  k24, k25],
                           [-eal,0.0, 0.0, eal,  0.0, 0.0],
                           [0.0, k14, k24, 0.0,  k44, k45],
                           [0.0, k15, k25, 0.0,  k45, k55]]
    tell $ printf "ee: %.2f, ii: %.8f, len: %.2f, e*i/l: %.6f" ee ii len b
    return beamK
    where aa = P.area bt'
          ee = P.emodulus bt'
          ii = P.secondAreaMoment bt'
          (N.Geom c s len) = N.calcGeom n1' n2'
          eal = ee*aa/len
          b = ee*ii/(len**3.0)
          k11 = 12*b
          k12 = (-6)*b*len  
          k14 = (-12)*b
          k15 = k12
          k22 = 4*b*len**2
          k24 = 6*b*len
          k25 = 2*b*len**2
          k44 = 12*b
          k45 = 6*b*len
          k55 = 4*b*len**2
#else
createK :: P.Profile a => Beam a -> Matrix Double
createK (Bjlk11 _ n1' n2' bt' _) = fromLists [[eal,  0.0, 0.0, -eal, 0.0, 0.0],
                                              [0.0,  0.0, 0.0, 0.0,  0.0, 0.0],
                                              [0.0,  0.0, 0.0, 0.0,  0.0, 0.0],
                                              [-eal, 0.0, 0.0, eal,  0.0, 0.0],
                                              [0.0,  0.0, 0.0, 0.0,  0.0, 0.0],
                                              [0.0,  0.0, 0.0, 0.0,  0.0, 0.0]]
    where aa = P.area bt'
          ee = P.emodulus bt'
          (N.Geom _ _ len) = N.calcGeom n1' n2'
          eal = ee*aa/len
createK (Bjlk33 _ n1' n2' bt' _) = fromLists [[eal, 0.0, 0.0, -eal, 0.0, 0.0],
                                              [0.0, k11, k12, 0.0,  k14, k15],
                                              [0.0, k12, k22, 0.0,  k24, k25],
                                              [-eal,0.0, 0.0, eal,  0.0, 0.0],
                                              [0.0, k14, k24, 0.0,  k44, k45],
                                              [0.0, k15, k25, 0.0,  k45, k55]]
    where aa = P.area bt'
          ee = P.emodulus bt'
          ii = P.secondAreaMoment bt'
          (N.Geom _ _ len) = N.calcGeom n1' n2'
          eal = ee*aa/len
          b = ee*ii/(len**3.0)
          k11 = 12*b
          k12 = (-6)*b*len  
          k14 = (-12)*b
          k15 = k12
          k22 = 4*b*len**2
          k24 = 6*b*len
          k25 = 2*b*len**2
          k44 = 12*b
          k45 = 6*b*len
          k55 = 4*b*len**2
createK (Bjlk13 _ _ _ _ _) = undefined
createK (Bjlk31 _ _ _ _ _) = undefined
#endif

calcGeom :: P.Profile a => Beam a -> N.Geom
calcGeom beam = N.calcGeom n1' n2'
    where n1' = (n1 beam)
          n2' = (n2 beam)

    
tg :: P.Profile a => Beam a -> Matrix Double
tg beam = fromLists [[ c, s, 0,  0, 0, 0],
                    [-s, c, 0,  0, 0, 0],
                    [ 0, 0, 1,  0, 0, 0],
                    [ 0, 0, 0,  c, s, 0],
                    [ 0, 0, 0, -s, c, 0],
                    [ 0, 0, 0,  0, 0, 1]]
    where (N.Geom c s _) = calcGeom beam 

createSY :: P.Profile a => Beam a -> L.Load -> Vector Double
createSY beam load = fromList [s0,s1,s2,s3,s4,s5]
    where q1 = (L.qy1 load)
          q2 = (L.qy2 load)
          (N.Geom _ _ len) = calcGeom beam 
          f = len/60.0
          s0 = 0.0
          s1 = ((-21.0)*q1 - (9.0*q2)) * f
          s2 = ((3.0*len*q1) + (2.0*len*q2)) * f
          s3 = 0.0
          s4 = ((-9.0)*q1 - (21.0*q2)) * f
          s5 = ((-2.0*len*q1) - (3.0*len*q2)) * f


l2gMx :: P.Profile a => Beam a -> [N.Loc2glob]
l2gMx b = N.loc2globMapMx (n1 b) (n2 b)

indexSeeds :: P.Profile a => Beam a -> [N.IndexPair]
indexSeeds beam = N.indexSeeds (n1 beam) (n2 beam)

#ifdef RCS_DEBUG
add2systemK :: P.Profile a => STMatrix s Double -> Beam a -> Writer String (ST s ())
add2systemK m beam = do 
    let result = mapM_ mywrite $ l2gMx beam
    tell msg
    return result 
    where myk = trans curTg <> beamK <> curTg 
          (beamK,msg) = runWriter $ createK beam
#else
add2systemK :: P.Profile a => STMatrix s Double -> Beam a -> ST s ()
add2systemK m beam = mapM_ mywrite $ l2gMx beam
    where myk = trans curTg <> createK beam<> curTg 
#endif
          curTg = tg beam 
          mywrite (N.Loc2glob (N.MatrixCoord lr lc) (N.MatrixCoord gr gc)) = do 
                modifyMatrix m gr gc (\x -> x + v) 
                if gc /= gr then
                        modifyMatrix m gc gr (\x -> x + v) 
                    else
                        return ()
            where v = myk @@> (lr,lc) 
            
add2systemSY' :: P.Profile a => STVector s Double -> L.LoadVariant -> Beam a -> ST s ()
add2systemSY' vec loadVar beam = mapM_ mywrite $ indexSeeds beam 
    where syk = trans (tg beam) <> createSY beam load
          load | loadVar == L.WoFact = fromJust $ ld beam
               | otherwise =  L.cloneWithFactor $ fromJust $ ld beam
          mywrite (loc,glob) = modifyVector vec glob (\x -> x - v)
                where v = syk @> loc 

add2systemSY :: P.Profile a => STVector s Double -> L.LoadVariant -> Beam a -> ST s ()
add2systemSY vec loadVar beam = case ld beam of Nothing -> return ()
                                                Just _ -> add2systemSY' vec loadVar beam 


localDisps :: P.Profile a => Beam a -> Vector Double -> Vector Double
localDisps beam globalDisps = mytg <> 6 |> [v0,v1,v2,v3,v4,v5]
    where v0 = findVal 0  
          v1 = findVal 1
          v2 = findVal 2
          v3 = findVal 3
          v4 = findVal 4
          v5 = findVal 5
          seeds = indexSeeds beam
          mytg = tg beam
          hit ix = find (\x -> fst x == ix) seeds 
          findVal i = case hit i of Nothing -> 0.0
                                    Just (_,gi) -> globalDisps @> gi

localForces :: P.Profile a => Beam a -> Vector Double -> Vector Double
localForces beam locV = case ld beam of Nothing -> locK <> locV
                                        Just load -> (locK <> locV) + (createSY beam load)
#ifdef RCS_DEBUG
    where (locK,msg) = runWriter $ createK beam 
#else
    where locK = createK beam 
#endif


localStresses :: P.Profile a => Beam a -> Vector Double -> (M.Stress,M.Stress)
localStresses beam forces = (a,b)
    where a = M.Stress (P.sigma curBt m1) (P.tau curBt v1)
          b = M.Stress (P.sigma curBt m2) (P.tau curBt v2)
          curBt = bt beam
          v1 = forces @> 1
          v2 = forces @> 4
          m1 = forces @> 2
          m2 = forces @> 5
