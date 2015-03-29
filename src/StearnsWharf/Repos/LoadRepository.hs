{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.LoadRepository where

import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (connectPostgreSQL,query,query_,close)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..))

import qualified StearnsWharf.Loads as L


instance FromRow L.Load where
    fromRow = L.Load <$> field <*> field <*> field <*> field <*> field <*> field 

fetchDistLoads :: Integer -> -- ^ System Id
                  IO [L.Load]
fetchDistLoads sysId = 
    connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'" >>= \c -> 
    (query c "select oid,qy1,qy2,qx1,qx2,lf from construction.dist_loads where sys_id=?" [sysId]) :: IO [L.Load]
