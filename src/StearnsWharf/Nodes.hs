{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Nodes where

import qualified Data.Map as Map

type NodeId = Int

type Index = Int

type LocalIndex = Int 

type GlobalIndex = Int 

type IndexPair = (LocalIndex,GlobalIndex)

data NodeType = FirstNode | SecondNode deriving Eq

data MatrixCoord = MatrixCoord { row, col :: Index } deriving Show

data Loc2glob = Loc2glob { loc, glob :: MatrixCoord } deriving Show
                
data Loc2globVec = Loc2globVec { locv, globv :: Int } deriving Show

data Dof = Dof { dofX, dofZ, dofM :: Int } deriving (Eq,Show)

data Node = Node {  nodeId :: NodeId, 
                    desc :: String, 
                    nx :: Double, 
                    ny :: Double,
                    nz :: Double,
                    dof :: Dof,  -- degrees of freedom
                    globNdx :: Index -- global index
                } 
            deriving (Show)

type NodeMap = Map.Map Int Node

instance Eq Node where
    (==) n1 n2 = (globNdx n1) == (globNdx n2)

instance Ord Node where
    compare n1 n2 = compare (globNdx n1) (globNdx n2)

data Geom = Geom { c :: Double, s :: Double, len :: Double } deriving Show

calcGeom :: Node -> Node -> Geom
calcGeom n1 n2 = Geom { c = xcos, 
                        s = xsin,
                        len =  zlen }
            where xDelta = (nx n2) - (nx n1) 
                  yDelta = (ny n2) - (ny n1) 
                  zDelta = (nz n2) - (nz n1) 
                  zlen = sqrt (xDelta**2.0 + yDelta**2.0 + zDelta**2.0)
                  xlen = sqrt (xDelta**2.0 + yDelta**2.0)
                  xcos = xlen / zlen  
                  xsin = zDelta / xlen  

systemIndexX :: Node -> Maybe Int
systemIndexX (Node _ _ _ _ _ (Dof x' _ _) gi) | x' == 0 = Nothing
                                          | otherwise = Just gi

systemIndexY :: Node -> Maybe Int
systemIndexY (Node _ _ _ _ _ (Dof x' y' _) gi) | y' == 0 = Nothing
                                           | x' == 0 = Just gi 
                                           | otherwise = Just $ gi + 1

numDof :: Dof -> Int
numDof d = (dofX d) + (dofZ d) + (dofM d) 

l2gMx :: IndexPair -> IndexPair -> Loc2glob
l2gMx a b = Loc2glob co1 co2 
    where co1 = MatrixCoord (fst a) (fst b)
          co2 = MatrixCoord (snd a) (snd b)

loc2globMapMx :: Node -> Node -> [Loc2glob]
loc2globMapMx n1 n2 = gx seeds 
    where seeds = indexSeeds n1 n2 
          gx [] = []
          gx (x:xs) = l2gMx x x : map l2g' xs ++ gx xs
                where l2g' = l2gMx x

bitSum :: Dof -> Int
bitSum (Dof d1 d2 d3) = d1 + 2*d2 + 4*d3

indexSeed :: Dof -> NodeType -> Index -> [IndexPair]
indexSeed d nodeType globalIndex = case bs of 
                                0 -> []
                                1 -> [xp]
                                2 -> [yp]
                                3 -> [xp,yp]
                                4 -> [zp]
                                5 -> [xp,zp]
                                6 -> [yp,zp]
                                7 -> [xp,yp,zp]
                                _ -> undefined
    where bs = bitSum d 
          xp = (startIndex,globalIndex)
          yp = (startIndex+1,globalIndexY)
          zp = (startIndex+2,globalIndexM)
          startIndex | nodeType == FirstNode = 0
                     | otherwise = 3                  
          globalIndexY = globalIndex + (dofX d)
          globalIndexM = globalIndexY + (dofZ d)


indexSeeds :: Node -> Node -> [IndexPair]
indexSeeds n1 n2 = foldr (:) ip2 ip1
    where ip1 = indexSeed d1 FirstNode $ globNdx n1
          ip2 = indexSeed d2 SecondNode $ globNdx n2 
          d1 = dof n1
          d2 = dof n2

-- | Clones node clo and setting global index to gi
clone :: Node     -- ^ Node to be cloned
         -> Int   -- ^ Global index to be used for the clone
         -> Node
clone clo gi = Node (nodeId clo) (desc clo) (nx clo) (ny clo) (nz clo) (dof clo) gi

