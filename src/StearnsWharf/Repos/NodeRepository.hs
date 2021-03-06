{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedStrings #-}


module StearnsWharf.Repos.NodeRepository where

import Text.Printf (printf)
import qualified Data.ByteString.UTF8 as UTF8 
import qualified Data.Map as Map
import Control.Monad.State (State,runState,get,put)
import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified StearnsWharf.Nodes as N


type NodeDef = (Int,N.Node)

instance FromRow N.Node where
    fromRow = N.Node <$> field <*> field <*> field <*> field <*> field <*> liftM3 N.Dof field field field <*> field

sql :: Query
sql = Query (UTF8.fromString (printf "%s union %s union %s union %s order by 3,4" s1 s2 w1 w2 :: String))
    where s1 = "select n.oid,dsc,n.x,n.y,n.z,n.dofx,n.dofz,n.dofm,0 from construction.nodes n join construction.steel_elements e on n.oid=e.n1 where e.sys_id=?"
          s2 = "select n.oid,dsc,n.x,n.y,n.z,n.dofx,n.dofz,n.dofm,0 from construction.nodes n join construction.steel_elements e on n.oid=e.n2 where e.sys_id=?"
          w1 = "select n.oid,dsc,n.x,n.y,n.z,n.dofx,n.dofz,n.dofm,0 from construction.nodes n join construction.wood_elements e on n.oid=e.n1 where e.sys_id=?"
          w2 = "select n.oid,dsc,n.x,n.y,n.z,n.dofx,n.dofz,n.dofm,0 from construction.nodes n join construction.wood_elements e on n.oid=e.n2 where e.sys_id=?"


fetchNodes :: Connection 
              -> Int           -- ^ System Id 
              -> IO [N.Node]
fetchNodes conn sysId = 
    (query conn sql [sysId,sysId,sysId,sysId]) :: IO [N.Node]

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
    
