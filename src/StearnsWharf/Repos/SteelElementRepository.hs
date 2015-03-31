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

import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Steel.SteelProfiles as S

data SteelElement = 
    SteelElement {
        elId :: Int,
        n1 :: Int,
        n2 :: Int,
        profileId :: Int,
        loadId :: Maybe Int } deriving Show

instance FromRow S.SteelProfile where
    fromRow = S.SteelProfile <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> liftM3 M.Steel field field field

instance FromRow SteelElement where
    fromRow = SteelElement <$> field <*> field <*> field <*> field <*> field 

systemSteelDTO:: Int -- ^ System Id
                 -> Connection
                 -> IO [SteelElement]
systemSteelDTO sysId conn = 
    (query conn "select oid,n1,n2,p_oid,ld_id from construction.v_steel_elements where sys_id=? order by x1,y1" [sysId]) :: IO [SteelElement]

steelElement2Beam :: N.NodeMap
                     -> L.LoadMap
                     -> S.SteelProfileMap 
                     -> SteelElement 
                     -> B.Beam S.SteelProfile
steelElement2Beam nm lm steelm el = B.Bjlk33 (elId el) n1' n2' steel ld
    where Just n1' = Map.lookup (n1 el) nm
          Just n2' = Map.lookup (n2 el) nm
          Just steel = Map.lookup (profileId el) steelm
          ld = loadId el >>= flip Map.lookup lm

systemSteelElements :: Int -- ^ System Id
                       -> Connection
                       -> N.NodeMap
                       -> L.LoadMap
                       -> IO [B.Beam S.SteelProfile]
systemSteelElements sysId conn nm lm = 
    systemSteelDTO sysId conn >>= \dto ->
    systemSteelAsMap sysId conn >>= \steelm ->
    let steelElement2Beam' = steelElement2Beam nm lm steelm in 
    return (map steelElement2Beam' dto)

systemSteelProfiles :: Int -- ^ System Id 
                       -> Connection
                       -> IO [S.SteelProfile]
systemSteelProfiles sysId conn = 
    (query conn 
        "select x.oid,x.name,x.b,x.h,x.area,x.w_el_y,x.i_y,200000.0 as emodule,355.0 as sigma,251.0 as tau from construction.steel_beams x join construction.steel_elements e on e.profile_id=x.oid where e.sys_id=?" [sysId]) 
        :: IO [S.SteelProfile]

systemSteelAsMap :: Int -- ^ System Id 
                    -> Connection
                    -> IO S.SteelProfileMap 
systemSteelAsMap sysId conn = 
    systemSteelProfiles sysId conn >>= \profiles ->
    return (Map.fromList (map asListItem profiles))
        where asListItem x = (S.profileId x, x)

