{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}
-- #define RCS_DEBUG

module StearnsWharf.Steel.HUP where

import StearnsWharf.Common (samr)
import qualified StearnsWharf.Profiles as P

data HUP = HUP { 
                 b :: Double,
                 h :: Double,
                 t :: Double }
                    deriving Show

instance P.Profile HUP where
    sigma    hp m = 255.0 
    tau      hp s = 4.0 
    area     HUP { b,h,t } = outerA - innerA
        where outerA = bu * hu
              innerA = bi * hi
              bu = b/1000.0
              hu = h/1000.0
              t2 = 2*t/1000.0
              bi = bu - t2
              hi = hu - t2
    emodulus hp   = 210000.0 
    sectionModulus hp = 5000.0 
    secondAreaMoment (HUP {b,h,t}) = outerI - innerI
        where outerI = samr bu hu
              bu = b/1000.0
              hu = h/1000.0
              t2 = 2*t/1000.0
              bi = bu - t2
              hi = hu - t2
              innerI = samr bi hi
#ifdef RCS_DEBUG
    centroid HUP { h } = undefined
#else
    centroid HUP { h } = h / 2000.0
#endif
        
