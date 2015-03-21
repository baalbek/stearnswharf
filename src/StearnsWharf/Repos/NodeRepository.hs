{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module StearnsWharf.Repos.NodeRepository where

import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (connectPostgreSQL,query,query_,close)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..))

import qualified StearnsWharf.Nodes as N


instance FromRow N.Node where
    fromRow = N.Node <$> field <*> field <*> field <*> liftM3 N.Dof field field field <*> field

fetchNodes :: Integer -> -- ^ System Id
              IO [N.Node]
fetchNodes sysId = 
    connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'" >>= \c -> 
    (query c "select oid,x,y,dofx,dofy,dofm,0 from construction.nodes where sys_id=?" [sysId]) :: IO [N.Node]
