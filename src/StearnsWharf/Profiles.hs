{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards  #-}
module StearnsWharf.Profiles where

-- #define RCS_DEBUG
#ifdef RCS_DEBUG
import Control.Monad.Writer (Writer,runWriter,tell)
#endif

--import qualified Matstat.Common as C
import StearnsWharf.Common (StaticMoment,Shear)

class Profile a where 
    desc               :: a -> String                     -- ^ Beskrivele av bjelke, mest brukt for SteelProfile
    desc _ = "N/A"
    sigma              :: a -> StaticMoment -> Double     -- ^ Boyespenninger for tverrsnittet [a -> kNm -> N/mm2]
    tau                :: a -> Shear -> Double            -- ^ Skjaer for tverrsnittet [a -> kN -> N/mm2] 
    area               :: a -> Double                     -- ^ Tverrsnittsareal [m2]
    areaMM             :: a -> Double                     -- ^ Tverrsnittsareal [mm2]
    areaMM x = 1000.0**2 * area x                         -- ^ Default implementation areaMM
    emodulus           :: a -> Double                     -- ^ Tverrsnittets emodul [N/mm2]
    secondAreaMoment   :: a -> Double                     -- ^ Annet arealmoment [m4]  
    secondAreaMomentMM :: a -> Double                     -- ^ Annet arealmoment [mm4]  
    secondAreaMomentMM x = 1000.0**4 * (secondAreaMoment x) -- ^ Default implementation secondAreaMomentMM 
    sectionModulus     :: a -> Double                     -- ^ Statisk moment, W [m3]
    sectionModulusMM   :: a -> Double                     -- ^ Statisk moment, W [mm3]
    sectionModulusMM x = 1000.0**3 * (sectionModulus x)   -- ^ Default implementation sectionModulusMM 
#ifdef RCS_DEBUG
    centroid           :: a -> Writer String Double       -- ^ Profilet centroid [m]
#else
    centroid           :: a -> Double                     -- ^ Profilet centroid [m]
    centroidMM         :: a -> Double                     -- ^ Profilet centroid [mm]
    centroidMM x = (centroid x) * 1000
#endif
    -- | Nedbøyning fra jevnt fordelt last [a -> kN -> m -> mm]
    deflection         :: a  
                          -> Double -- ^ Jevnt fordelt last q [kN/m]  
                          -> Double -- ^ Elementlengde l [m] 
                          -> Double -- ^ Nedbøyning [mm]          
    deflection x q l = 5 * q * (l'**4) / (384 * ee * ii)
        where ee = emodulus x
              l' = l * 1000.0
              ii = secondAreaMomentMM x
         
        


