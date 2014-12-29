{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}
-- #define RCS_DEBUG

module StearnsWharf.Steel.Plates where

import StearnsWharf.Common (BeamTypeId,famr,samr)
import qualified StearnsWharf.Profiles as P

data Plate = Plate {    
                        b :: Double, -- ^ Bredde på tynnplateprofil
                        h :: Double  -- ^ Tykkelse på tynnplateprofil
                    } deriving Show


instance P.Profile Plate where
    sigma    p m = m / (1000.0 * (P.sectionModulus p))
    tau      p s = 4.0 
    area     Plate {b, h} = b' * h'
        where b' = b/1000.0
              h' = h/1000.0
    emodulus p   = 200000.0
    sectionModulus Plate { b,h } = famr b' h'
        where b' = b/1000.0
              h' = h/1000.0
    secondAreaMoment Plate { b, h} = samr b' h'
        where b' = b/1000.0
              h' = h/1000.0
    centroid Plate { h } = h / 2000.0
