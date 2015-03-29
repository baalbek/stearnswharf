{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.SteelElementRepository where

import Control.Monad (mplus,liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple.Internal (Connection)
import Database.PostgreSQL.Simple (connectPostgreSQL,query,query_,close)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..),In(..))

import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Materials as M
import StearnsWharf.Steel.SteelProfiles (SteelProfile(..))


instance FromRow SteelProfile where
    fromRow = SteelProfile <$> field <*> field <*> field <*> field <*> field <*> field <*> liftM3 M.Steel field field field

systemSteelProfiles :: [Integer]  -- ^ steel_beams.oid
                       -> Maybe (IO Connection)
                       -> IO [SteelProfile]
systemSteelProfiles keys conn = 
    let Just conn' = mplus conn (Just (connectPostgreSQL "host='xxochitecatl2' dbname='engineer' user='engineer'")) in
    conn' >>= \c -> 
    (query c "select name,b,h,area,w_el_y,i_y,200000.0 as emodule,355.0 as sigma,251.0 as tau from construction.steel_beams where oid in ?" 
        (Only (In keys))) :: IO [SteelProfile]


