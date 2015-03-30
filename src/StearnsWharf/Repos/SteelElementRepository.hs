{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.SteelElementRepository where

import qualified Data.Map as Map
import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..),In(..))

import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Steel.SteelProfiles as S

instance FromRow S.SteelProfile where
    fromRow = S.SteelProfile <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> liftM3 M.Steel field field field

systemSteelProfiles :: [Int]  -- ^ steel_beams.oid
                       -> Connection
                       -> IO [S.SteelProfile]
systemSteelProfiles keys conn = 
    (query conn "select oid,name,b,h,area,w_el_y,i_y,200000.0 as emodule,355.0 as sigma,251.0 as tau from construction.steel_beams where oid in ?" 
        (Only (In keys))) :: IO [S.SteelProfile]

uniqueSystemSteelIds :: Int -- ^ System Id 
                      -> Connection 
                      -> IO [Int]
uniqueSystemSteelIds sysId conn = 
    ((query conn "select distinct(profile_id) from construction.steel_elements where sys_id=?" [sysId]) :: IO [Only Int]) >>= \x ->
    return (map fromOnly x)

systemSteelAsMap :: Int -- ^ System Id 
                    -> Connection
                    -> IO SteelProfileMap 
systemSteelAsMap sysId conn = 
    (uniqueSystemSteelIds sysId conn) >>= \uc ->
    systemSteelProfiles uc conn >>= \profiles ->
    return (Map.fromList (map asListItem profiles))
        where asListItem x = (S.profileId x, x)

