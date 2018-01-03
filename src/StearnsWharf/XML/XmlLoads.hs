{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.XML.XmlLoads where

import qualified Data.Map as Map
import qualified Text.XML.Light as X
import qualified StearnsWharf.XML.Common as XC
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Nodes as N

type LoadDef = (String,L.Load)
type LoadVectors = (Double,Double)
type LoadMap = Map.Map String L.Load
data LoadDirection = LDY | LDX deriving Eq

findDistLoads :: X.Element -> LoadDirection -> LoadVectors
findDistLoads el loadDir = result
    where y' = XC.xmlAttr ys el
          result = case y' of Nothing -> (y1, y2)
                                where y1 = maybeLoad ys1
                                      y2 = maybeLoad ys2
                              Just v -> (y1,y1)
                                where y1 = read v
          maybeLoad v = maybe 0.0 (\p -> read p) $ XC.xmlAttr v el
          (ys,ys1,ys2) | loadDir == LDY = ("y","y1","y2")
                       | otherwise = ("x","x1","x2")

loadDef :: X.Element -> LoadDef
loadDef el = (lid, (L.Load 1 y1 y2 x1 x2 loadfactor))
    where Just lid = XC.xmlAttr "id" el
          Just loadfactor = XC.xmlAttr "f" el >>= Just . read
          (y1,y2) = findDistLoads el $ LDY
          (x1,x2) = findDistLoads el $ LDX

createLoads :: X.Element -> LoadMap
createLoads doc = Map.fromList loadDefs
    where xmlloads = XC.xmlElements "load" doc
          loadDefs = map loadDef xmlloads

createPointLoad :: N.NodeMap -> X.Element -> L.PointLoad
createPointLoad nm el = L.PointLoad 1 v node ang f
    where Just f = XC.xmlAttr "f" el >>= Just . read
          Just v = XC.xmlAttr "v" el >>= Just . read
          Just ang = XC.xmlAttr "ang" el >>= Just . read
          Just nid = XC.xmlAttr "node" el
          Just node = Map.lookup (read nid) nm

createPointLoads :: N.NodeMap -> X.Element -> [L.PointLoad]
createPointLoads nm el = map createPointLoad' xmlploads
    where xmlploads = X.findElements (X.unqual "pointload") el
          createPointLoad' = createPointLoad nm
