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
        ar :: Int,
        wely :: PgNum,
        iiy :: PgNum,
        matr :: M.Material }
    deriving Show

type SteelProfileMap = Map.Map Int SteelProfile

instance P.Profile SteelProfile where
    desc     (SteelProfile {name}) = name
    sigma    hp moment = moment / (1000.0 * (P.sectionModulus hp)) 
    tau      hp shr = (3.0*shr) / (2000.0 * (P.area hp))
    area     (SteelProfile {ar}) = fromIntegral ar 
    emodulus hp   = 1000 * ((fromRational . M.emodulus2 . matr) hp)
    sectionModulus (SteelProfile {wely}) = fromRational wely
    secondAreaMoment (SteelProfile {iiy}) = fromRational iiy
    centroid _ = undefined

