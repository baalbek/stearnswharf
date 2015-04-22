{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.WoodElementRepository where

import Control.Applicative ((<$>),(<*>))

import qualified Data.Map as Map

import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified StearnsWharf.Wood.WoodProfiles as W
import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Beams as B

{-
data WoodElementDTO = 
    WoodElementDTO {
    } deriving Show
-}

data WoodProfileDTO = 
        WoodProfileDTO {
            elId :: Int,
            n1 :: Int, 
            n2 :: Int, 
            stClass :: String,
            width :: Double,
            height :: Double,
            fmk :: Double,
            fvk :: Double,
            emodulus :: Double,
            loadId :: Maybe Int
        }
        deriving Show

instance FromRow WoodProfileDTO where
    fromRow = WoodProfileDTO <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

systemWoodDTO :: Connection
                 -> Int    -- ^ System Id
                 -> IO [WoodProfileDTO]
systemWoodDTO conn sysId =
    (query conn "select oid,n1,n2,class_name,w,h,fmk,fvk,e0mean,ld_id from construction.v_wood_elements where sys_id=? order by x1,y1" [sysId]) :: IO [WoodProfileDTO]

dto2Beam :: N.NodeMap
            -> L.LoadMap
            -> WoodProfileDTO 
            -> B.Beam W.WoodProfile
dto2Beam nm lm dto = B.Bjlk33 (elId dto) n1' n2' wood ld
    where Just n1' = Map.lookup (n1 dto) nm
          Just n2' = Map.lookup (n2 dto) nm
          wood = W.WoodProfile (stClass dto) (width dto) (height dto) (fmk dto) (fvk dto) (emodulus dto)
          ld = loadId dto >>= flip Map.lookup lm

systemWoodElements :: Connection
                      -> Int    -- ^ System Id
                      -> N.NodeMap
                      -> L.LoadMap
                      -> IO [B.Beam W.WoodProfile]
systemWoodElements conn sysId nm lm = 
    systemWoodDTO conn sysId >>= \dtos ->
    let dto2Beam' = dto2Beam nm lm in 
    return (map dto2Beam' dtos)

