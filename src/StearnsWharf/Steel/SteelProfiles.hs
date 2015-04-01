{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards,OverloadedStrings #-}
-- #define RCS_DEBUG

module StearnsWharf.Steel.SteelProfiles where

import Data.Ratio (Ratio)

import qualified Data.Map as Map
import qualified StearnsWharf.Profiles as P
import qualified StearnsWharf.Materials as M

type PgNum = Ratio Integer 

type SteelProfileId = Int

data SteelProfile = 
    SteelProfile { 
        profileId :: SteelProfileId, 
        name :: String,
        b :: Int,
        h :: Int,
        flange :: PgNum,
        web :: PgNum,
        wely :: PgNum,
        iiy :: PgNum,
        matr :: M.Material }
    deriving Show

type SteelProfileMap = Map.Map Int SteelProfile

instance P.Profile SteelProfile where
    desc     (SteelProfile {name}) = name
    sigma    hp moment = moment / (1000.0 * (P.sectionModulus hp)) 
    tau      hp shr = (3.0*shr) / (2000.0 * (P.area hp))
    area     (SteelProfile { h,flange,web }) = ((h' - flange') * web') / 1000000.0
        where flange' = 2 * (fromRational flange)
              web' = fromRational web
              h' = fromIntegral h 
    emodulus hp   = 1000 * ((fromRational . M.emodulus2 . matr) hp)
    sectionModulus (SteelProfile {wely}) = (fromRational wely) / 1000000.0
    secondAreaMoment (SteelProfile {iiy}) = (fromRational iiy) / 100000000.0
    centroid _ = undefined
