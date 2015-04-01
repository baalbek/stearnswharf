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
    fromRow = N.Node <$> field <*> field <*> field <*> liftM3 N.Dof field field field <*> field

fetchNodes :: Int           -- ^ System Id
              -> Connection 
              -> IO [N.Node]
fetchNodes sysId conn = 
    (query conn "select oid,x,y,dofx,dofy,dofm,0 from construction.nodes where sys_id=?" [sysId]) :: IO [N.Node]

matstatNodeDef :: N.Node -> State Int NodeDef
matstatNodeDef node = 
    get >>= \j ->
    put (j + (N.numDof (N.dof node))) >> 
    return ((N.nodeId node), (N.clone node j))

linkNodes :: [N.Node] -> Int -> [NodeDef]
linkNodes [] _ = []
linkNodes (x:xs) j = fst rs : linkNodes xs (snd rs)
    where rs = runState (matstatNodeDef x) j

fetchNodesAsMap :: Int  -- ^ System Id
                   -> Connection 
                   -> IO N.NodeMap
fetchNodesAsMap sysId conn = fetchNodes sysId conn >>= \nodes ->
    return (Map.fromList (linkNodes nodes 0))
    
