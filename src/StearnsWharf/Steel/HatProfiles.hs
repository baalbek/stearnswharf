{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}
-- #define RCS_DEBUG

module StearnsWharf.Steel.HatProfiles where

import StearnsWharf.Common (BeamTypeId)
import qualified StearnsWharf.Profiles as P
import qualified StearnsWharf.Steel.HUP as H
import qualified StearnsWharf.Steel.Plates as Pl


data HatProfile 
        -- | Hatteprofil m/ HUP og gj.gående bunnplate
        = HatProfileA { 
                        hup :: H.HUP,
                        plate :: Pl.Plate }
        -- | Norsk Stålforbund utviklet to-/ensidig hatteprofil 
        --   eks. tosidig: THP 250 x 6 - 200 x 30 - 462 x 15
        --   eks. ensidig: EHP 250 x 6 - 200 x 30 - 352 x 15
        | TEHP { 
            topPl :: Pl.Plate,   -- ^ Topplate 
            webPl :: Pl.Plate,   -- ^ Stegplater, behandles som to stk selv om angitt som en
            botPl :: Pl.Plate    -- ^ Bunnplate
        }
        deriving Show

createTEHP :: 
              Double -- ^ Høyde for hatten over bunnplate [mm]
              -> Double -- ^ Tykkelse for stegplater [mm]
              -> Double -- ^ Bredde for topplate [mm]
              -> Double -- ^ Tykkelse for topplate [mm] 
              -> Double -- ^ Bredde for bunnplate [mm]
              -> Double -- ^ Tykkelse for bunnplate [mm] 
              -> HatProfile
createTEHP h tw bo to bu tu  = TEHP topP webP botP
    where topP = Pl.Plate "top" bo to
          webP = Pl.Plate "web" tw (h - tw)
          botP = Pl.Plate "bot" bu tu   

instance P.Profile HatProfile where
    sigma    hp m = m / (1000.0 * (P.sectionModulus hp))
    tau      hp s = 4.0 
    area     HatProfileA { hup,plate } = (P.area hup) + (P.area plate)
    emodulus hp   = 210000.0
    sectionModulus hat@HatProfileA { hup, plate } = sam / y 
        where sam = P.secondAreaMoment hat
              y = max c cy
              cy = hh + ph - c 
              c = P.centroid hat
              hh = (H.h hup) / 1000.0
              ph = (Pl.h plate) / 1000.0
    sectionModulus hat@TEHP { .. } = sam / y
        where sam = P.secondAreaMoment hat
              cc = P.centroid hat
              hh = totalHeight hat 
              y | hh / 2.0 > cc = hh - cc
                | otherwise = cc
    secondAreaMoment hat@(HatProfileA { hup, plate }) = huI + plateI + cenHupI + cenPlateI
        where huI = P.secondAreaMoment hup
              plateI = P.secondAreaMoment plate
              c = P.centroid hat
              cenHup = P.centroid hup
              cenPlate = P.centroid plate
              cenHupI = (P.area hup) * ((c - cenHup)**2)
              cenPlateI = P.area plate * ((c - cenPlate)**2)
    secondAreaMoment hat@TEHP { .. } = ii + (at*dt*dt) + (ab*db*db) + (aw*dw*dw)
        where ii = (P.secondAreaMoment topPl) + ((P.secondAreaMoment webPl)*2.0) + (P.secondAreaMoment botPl)
              hb = (Pl.h botPl) / 1000.0 
              -- hw = (Pl.h webPl) / 1000.0
              hw = webHeight hat 
              at = P.area topPl
              aw = 2.0 * (P.area webPl)
              ab = P.area botPl
              cc = P.centroid hat
              dt = hb + hw - cc - (P.centroid topPl) -- ^ distance form centroid top to main centroid
              dw = (P.centroid webPl) + hb - cc  -- ^ distance form centroid web to main centroid
              db = cc - (P.centroid botPl)  -- ^ distance form centroid bottom to main centroid

#ifdef RCS_DEBUG
    centroid hat@HatProfileA { hup,plate } = (pca + huca) / (P.area hat)
        where pc = P.centroid plate
              huc = (P.centroid hup)  + ((Pl.h plate) / 1000.0)
              pca = pc * (P.area plate)
              huca = huc * (P.area hup)
    centroid hat@TEHP { .. } = ((ct*at) + (cw*aw) + (cb*ab)) / area
        where hb = (Pl.h botPl) / 1000.0
              hw = webHeight hat 
              ct = hw + hb - (P.centroid topPl)
              cw = (P.centroid webPl) + ((Pl.h botPl) / 2000.0)
              cb = P.centroid botPl
              at = P.area topPl
              aw = 2.0 * (P.area webPl)
              ab = P.area botPl
              area = at + aw + ab
#else
    centroid hat = undefined
#endif
              
-- | Total høyde for profilet
totalHeight :: HatProfile -> Double
totalHeight TEHP { webPl,botPl } = (Pl.h webPl + Pl.h botPl) / 1000.0

-- | Total høyde på "hatten" over bunnplaten
webHeight :: HatProfile -> Double
webHeight TEHP { webPl } = (Pl.h webPl + Pl.b webPl) / 1000.0

jax :: Int -> Int
#ifdef RCS_DEBUG
jax v = 2*v
#else
jax v = 22*v
#endif
