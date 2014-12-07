{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}

module StearnsWharf.Wood.WoodProfiles where

import qualified StearnsWharf.Profiles as P
import qualified StearnsWharf.Materials as M

data WoodProfile = 
        WoodProfile {
            width :: Double,
            height :: Double,
            matr :: M.Material
        }
        deriving Show

newWoodProfile :: String     -- ^ Strength Class 
           -> Double  -- ^ Width
           -> Double  -- ^ Height
           -> WoodProfile 
newWoodProfile stc w h = case stc of 
    "C16" -> WoodProfile w h (M.Wood 8000 16 3.2 False stc)
    "C22" -> WoodProfile w h (M.Wood 10000 22 3.8 False stc)
    "C24" -> WoodProfile w h (M.Wood 10000 24 4.0 False stc)
    "CEL30C" -> WoodProfile w h (M.Wood 14000 30 4.0 True stc)
    "CEL40C" -> WoodProfile w h (M.Wood 14000 40 4.0 True stc)
    _ -> undefined 

instance P.Profile WoodProfile where
    desc WoodProfile { matr } = "Wood Profile " ++ (M.stClass matr)
    sigma          wp moment = moment / (1000.0 * (P.sectionModulus wp)) 
    tau            wp shr = (3.0*shr) / (2000.0 * (P.area wp))
    area           WoodProfile { width,height } = (width * height) / 1000000.0
    emodulus       WoodProfile { matr } = (M.emodulus matr) * 1000.0
    secondAreaMoment WoodProfile { width,height } = (width/1000) * (height/1000)**3 / 12.0
    sectionModulus   WoodProfile { width,height } = (width/1000) * (height/1000)**2 / 6.0
    centroid         WoodProfile { height } = height / 2000.0
