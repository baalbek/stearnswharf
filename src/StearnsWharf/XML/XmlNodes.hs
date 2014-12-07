{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.XML.XmlNodes (createMatstatNodes) where

import Control.Monad.State (State,runState,get,put)
import qualified Data.Map as Map
import qualified Text.XML.Light as X 
import qualified StearnsWharf.XML.Common as XC
import qualified StearnsWharf.Nodes as N

type NodeDef = (String,N.Node)

parseDof :: String -> N.Dof 
parseDof s = case s of "0" -> N.Dof 0 0 0
                       "1" -> N.Dof 0 0 1 
                       "2" -> N.Dof 0 1 0 
                       "3" -> N.Dof 0 1 1
                       "4" -> N.Dof 1 0 0
                       "5" -> N.Dof 1 0 1
                       "6" -> N.Dof 1 1 0
                       "7" -> N.Dof 1 1 1
                       _ -> undefined

matstatNodeDef :: X.Element -> State Int NodeDef
matstatNodeDef el = do
    j <- get
    let Just nid = XC.xmlAttr "id" el 
    let Just xcoord = XC.xmlAttr "x" el >>= (\s -> Just (read s)) :: Maybe Double
    let ycoord = maybe 0.0 (\yv -> read yv) $ XC.xmlAttr "y" el 
    let Just dof = XC.xmlAttr "dof" el >>= (\xv -> Just $ parseDof xv)
    put $ j + (N.numDof dof) 
    return (nid,N.Node nid xcoord ycoord dof j)

genNodeDefs :: [X.Element] -> Int -> [NodeDef]
genNodeDefs [] _ = []
genNodeDefs (x:xs) j = fst rs : genNodeDefs xs (snd rs)
    where rs = runState (matstatNodeDef x) j

createMatstatNodes :: X.Element -> N.NodeMap
createMatstatNodes doc = Map.fromList nodeDefs 
    where nodeDefs = genNodeDefs (XC.xmlElements "node" doc) 0
