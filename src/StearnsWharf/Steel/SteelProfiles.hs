{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards,OverloadedStrings #-}
-- #define RCS_DEBUG

module StearnsWharf.Steel.SteelProfiles where

import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (connectPostgreSQL,query,query_,close)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..))
import Data.Ratio (Ratio)

import qualified Data.Map as Map
import qualified StearnsWharf.Profiles as P
import qualified StearnsWharf.Materials as M

type PgNum = Ratio Integer 

data SteelProfile = 
    SteelProfile { 
        name :: String,
        b :: Int,
        h :: Int,
        ar :: Int,
        wely :: PgNum,
        iiy :: PgNum,
        matr :: M.Material }
    deriving Show

instance P.Profile SteelProfile where
    desc     (SteelProfile {name}) = name
    sigma    hp moment = moment / (1000.0 * (P.sectionModulus hp)) 
    tau      hp shr = (3.0*shr) / (2000.0 * (P.area hp))
    area     (SteelProfile {ar}) = fromIntegral ar 
    emodulus hp   = 1000 * ((fromRational . M.emodulus2 . matr) hp)
    sectionModulus (SteelProfile {wely}) = fromRational wely
    secondAreaMoment (SteelProfile {iiy}) = fromRational iiy
    centroid _ = undefined

instance FromRow SteelProfile where
    fromRow = SteelProfile <$> field <*> field <*> field <*> field <*> field <*> field <*> liftM3 M.Steel field field field

steelProfileOf :: Integer -> IO SteelProfile
steelProfileOf key = do 
    c <- connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'"
    r <- (query c "select name,b,h,area,w_el_y,i_y,200000.0 as emodule,355.0 as sigma,251.0 as tau from construction.steel_beams where oid=?" [key]) :: IO [SteelProfile]
    return (head r)

