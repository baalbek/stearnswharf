{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module StearnsWharf.Repos.NodeRepository where

import qualified Data.Map as Map
import Control.Monad.State (State,runState,get,put)
import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import StearnsWharf.Common (getConnection)
import qualified StearnsWharf.Nodes as N


type NodeDef = (Int,N.Node)

instance FromRow N.Node where
    fromRow = N.Node <$> field <*> field <*> field <*> field <*> liftM3 N.Dof field field field <*> field

fetchNodes :: Connection 
              -> Int           -- ^ System Id 
              -> IO [N.Node]
fetchNodes conn sysId = 
    (query conn "select n.oid,n.x,n.y,n.z,n.dofx,n.dofz,n.dofm,0 from construction.nodes n join construction.systems s on s.project_id=n.project_id and s.coord_sys=n.coord_sys where s.oid=?" [sysId]) :: IO [N.Node]

matstatNodeDef :: N.Node -> State Int NodeDef
matstatNodeDef node = 
    get >>= \j ->
    put (j + (N.numDof (N.dof node))) >> 
    return ((N.nodeId node), (N.clone node j))

linkNodes :: [N.Node] -> Int -> [NodeDef]
linkNodes [] _ = []
linkNodes (x:xs) j = fst rs : linkNodes xs (snd rs)
    where rs = runState (matstatNodeDef x) j

fetchNodesAsMap :: Connection 
                   -> Int           -- ^ System id 
                   -> IO N.NodeMap
fetchNodesAsMap conn sysId = fetchNodes conn sysId >>= \nodes ->
    return (Map.fromList (linkNodes nodes 0))
    
