{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.LoadRepository where

import qualified Data.Map as Map
import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..))

import qualified StearnsWharf.Loads as L


instance FromRow L.Load where
    fromRow = L.Load <$> field <*> field <*> field <*> field <*> field <*> field 

fetchDistLoads :: Int       -- ^ System Id
                  -> Int    -- ^ Load Case 
                  -> Connection 
                  -> IO [L.Load]
fetchDistLoads sysId lc conn = 
    (query conn "select oid,qy1,qy2,qx1,qx2,lf from construction.dist_loads where sys_id=?" [sysId]) :: IO [L.Load]

fetchDistLoadsAsMap :: Int       -- ^ System Id
                       -> Int    -- ^ Load Case 
                       -> Connection 
                       -> IO L.LoadMap
fetchDistLoadsAsMap sysId lc conn = fetchDistLoads sysId lc conn >>= \loads ->
    return (Map.fromList (map asListItem loads))
        where asListItem x = (L.loadId x, x)
