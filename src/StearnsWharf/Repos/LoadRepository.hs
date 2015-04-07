{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Repos.LoadRepository where

import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (Connection,query)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)

import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Nodes as N

data PointLoadDTO = 
    PointLoadDTO {
        oid :: Int,
        n1 :: Int,
        p :: Double,
        lf :: Double,
        ang :: Double
    } deriving Show

instance FromRow PointLoadDTO where
    fromRow = PointLoadDTO <$> field <*> field <*> field <*> field <*> field 

instance FromRow L.Load where
    fromRow = L.Load <$> field <*> field <*> field <*> field <*> field <*> field 

fetchDistLoads :: Int       -- ^ System Id
                  -> Connection 
                  -> IO [L.Load]
fetchDistLoads sysId conn = 
    (query conn "select oid,qy1,qy2,qx1,qx2,lf from construction.dist_loads where sys_id=?" [sysId]) :: IO [L.Load]

fetchDistLoadsAsMap :: Int       -- ^ System Id
                       -> Connection 
                       -> IO L.LoadMap
fetchDistLoadsAsMap sysId conn = fetchDistLoads sysId conn >>= \loads ->
    return (Map.fromList (map asListItem loads))
        where asListItem x = (L.loadId x, x)


systemPointLoadDTOs :: Connection
                      -> Int       -- ^ System Id
                      -> IO [PointLoadDTO]
systemPointLoadDTOs conn sysId =
    (query conn "select oid,n1,p,lf,ang from construction.point_loads where sys_id=?" [sysId]) :: IO [PointLoadDTO]

systemPointLoads :: Connection
                    -> Int       -- ^ System Id
                    -> N.NodeMap
                    -> IO [L.PointLoad]
systemPointLoads conn sysId nm = systemPointLoadDTOs conn sysId >>= \dtos ->
    return (map asPointLoad dtos)
        where asPointLoad x = let Just node = Map.lookup (n1 x) nm in L.PointLoad (oid x) (p x) node (ang x) (lf x)

    

