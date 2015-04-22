{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}

module StearnsWharf.Wood.WoodProfiles where

import Text.Printf (printf)

import qualified StearnsWharf.Profiles as P

data WoodProfile = 
        WoodProfile {
            stClass :: String,
            width :: Double,
            height :: Double,
            fmk :: Double,
            fvk :: Double,
            emodulus :: Double
        }
        deriving Show

instance P.Profile WoodProfile where
    desc WoodProfile { width,height,stClass } = printf "Wood Profile %.0fx%.0fmm %s" width height stClass
    sigma          wp moment = moment / (1000.0 * (P.sectionModulus wp)) 
    tau            wp shr = (3.0*shr) / (2000.0 * (P.area wp))
    area           WoodProfile { width,height } = (width * height) / 1000000.0
    emodulus       WoodProfile { emodulus } =  emodulus * 1000.0
    secondAreaMoment WoodProfile { width,height } = (width/1000) * (height/1000)**3 / 12.0
    sectionModulus   WoodProfile { width,height } = (width/1000) * (height/1000)**2 / 6.0
    centroid         WoodProfile { height } = height / 2000.0
