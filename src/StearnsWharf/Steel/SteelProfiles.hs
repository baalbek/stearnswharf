{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards,OverloadedStrings #-}
-- #define RCS_DEBUG
-- #define RCS_POSTGRESQL

module StearnsWharf.Steel.SteelProfiles where

#ifdef RCS_POSTGRESQL
import Data.Ratio (Ratio)
import Control.Monad (liftM3)
import Control.Applicative ((<$>),(<*>))
import Database.PostgreSQL.Simple (connectPostgreSQL,query,query_,close)
import Database.PostgreSQL.Simple.FromRow (FromRow,fromRow,field)
import Database.PostgreSQL.Simple.Types (Only(..))
import Data.Ratio (Ratio)
#endif



import qualified Data.Map as Map
import qualified StearnsWharf.Profiles as P
import qualified StearnsWharf.Materials as M

#ifdef RCS_POSTGRESQL
type PgNum = Ratio Integer 
#endif

data SteelProfile = IBeam { 
                            width :: Double,
                            height :: Double,
                            matr   :: M.Material,
                            flange :: Double,
                            web :: Double }
#ifdef RCS_POSTGRESQL
                    | DBSteelProfile { 
                            name :: String,
                            b :: Int,
                            h :: Int,
                            ar :: Int,
                            wely :: PgNum,
                            iiy :: PgNum,
                            matr :: M.Material }
#else
                    | DBSteelProfile { 
                            name :: String, 
                            width  :: Double,
                            height :: Double,
                            aa :: Double,
                            ww :: Double,
                            ii :: Double,
                            matr :: M.Material } 
#endif
            deriving Show

newIbeam :: 
            Double ->         -- ^ Width [mm]
            Double ->         -- ^ Height [mm]
            M.Material ->      
            Double ->         -- ^ Flenstykkelse [mm] 
            Double ->         -- ^ Stegtykkelse [mm] 
            SteelProfile 
newIbeam w' h' matr' f' web' = IBeam (w'/1000.0)  (h'/1000.0) matr' (f'/1000.0) (web'/1000.0)



instance P.Profile SteelProfile where
    desc     (DBSteelProfile {name}) = name
    desc     (IBeam _ _ _ _ _) = "IBeam"
    sigma    hp moment = moment / (1000.0 * (P.sectionModulus hp)) 
    tau      hp shr = (3.0*shr) / (2000.0 * (P.area hp))
#ifdef RCS_POSTGRESQL
    area     (DBSteelProfile {ar}) = fromIntegral ar 
#else
    area     (DBSteelProfile {aa}) = aa
#endif
    area     (IBeam {width,height,flange,web}) = outerA - innerA
        where outerA = width*height
              b' = width - web
              h' = height - (2*flange)
              innerA = b' * h'
    emodulus hp   = 1000 * (M.emodulus $ matr hp)
#ifdef RCS_POSTGRESQL
    sectionModulus (DBSteelProfile {wely}) = fromRational wely
#else
    sectionModulus (DBSteelProfile {ww}) = ww
#endif
    sectionModulus (IBeam _ _ _ _ _) = undefined
#ifdef RCS_POSTGRESQL
    secondAreaMoment (DBSteelProfile {iiy}) = fromRational iiy
#else
    secondAreaMoment (DBSteelProfile {ii}) = ii
#endif
    secondAreaMoment (IBeam {width,height,flange,web}) = outerW - (2*innerW)
        where outerW = width*(height**2)/6.0
              b' = (width - web) / 2.0
              h' = height - (2*flange)
              innerW = b'*(h'**2) / 6.0
    centroid _ = undefined

#ifdef RCS_POSTGRESQL
instance FromRow SteelProfile where
    fromRow = DBSteelProfile <$> field <*> field <*> field <*> field <*> field <*> field <*> liftM3 M.DbSteel field field field

steelProfileOf :: Integer -> IO SteelProfile
steelProfileOf key = do 
    c <- connectPostgreSQL "host='xochitecatl2' dbname='engineer' user='engineer'"
    r <- (query c "select name,b,h,area,w_el_y,i_y,200000.0 as emodule,355.0 as sigma,251.0 as tau from construction.steel_beams where oid=?" [key]) :: IO [SteelProfile]
    return (head r)
#else 
steelProfileOf :: Int -> SteelProfile
steelProfileOf key = result 
    where Just result = Map.lookup key mySteelProfiles  

steelProfileOf2 :: String -> Maybe SteelProfile
steelProfileOf2 key = Map.lookup key' mySteelProfiles  
    where key' = read key

mySteelProfiles :: Map.Map Int SteelProfile
mySteelProfiles = Map.fromList [
            (1, DBSteelProfile "HE 100 AA" 100 91 0.00156 5.198e-05 2.365e-06 mysteel),
            (2, DBSteelProfile "HE 100 A" 100 96 0.00212 7.276e-05 3.492e-06 mysteel),
            (3, DBSteelProfile "HE 100 B" 100 100 0.0026 8.991e-05 4.495e-06 mysteel),
            (4, DBSteelProfile "HE 100 M" 106 120 0.00532 0.0001904 1.143e-05 mysteel),
            (5, DBSteelProfile "HE 120 AA" 120 109 0.00186 7.585e-05 4.134e-06 mysteel),
            (6, DBSteelProfile "HE 120 A" 120 114 0.00253 0.0001063 6.062e-06 mysteel),
            (7, DBSteelProfile "HE 120 B" 120 120 0.0034 0.0001441 8.644e-06 mysteel),
            (8, DBSteelProfile "HE 120 M" 126 140 0.00664 0.0002882 2.018e-05 mysteel),
            (9, DBSteelProfile "HE 140 AA" 140 128 0.0023 0.0001124 7.195e-06 mysteel),
            (10, DBSteelProfile "HE 140 A" 140 133 0.00314 0.0001554 1.033e-05 mysteel),
            (11, DBSteelProfile "HE 140 B" 140 140 0.0043 0.0002156 1.509e-05 mysteel),
            (12, DBSteelProfile "HE 140 M" 146 160 0.00806 0.0004114 3.291e-05 mysteel),
            (13, DBSteelProfile "HE 160 AA" 160 148 0.00304 0.0001734 1.283e-05 mysteel),
            (14, DBSteelProfile "HE 160 A" 160 152 0.00388 0.0002201 1.673e-05 mysteel),
            (15, DBSteelProfile "HE 160 B" 160 160 0.00543 0.0003115 2.492e-05 mysteel),
            (16, DBSteelProfile "HE 160 M" 166 180 0.00971 0.0005665 5.098e-05 mysteel),
            (17, DBSteelProfile "HE 180 AA" 180 167 0.00365 0.0002356 1.967e-05 mysteel),
            (18, DBSteelProfile "HE 180 A" 180 171 0.00453 0.0002936 2.51e-05 mysteel),
            (19, DBSteelProfile "HE 180 B" 180 180 0.00653 0.0004257 3.831e-05 mysteel),
            (20, DBSteelProfile "HE 180 M" 186 200 0.01133 0.0007483 7.483e-05 mysteel),
            (21, DBSteelProfile "HE 200 AA" 200 186 0.00441 0.0003166 2.944e-05 mysteel),
            (22, DBSteelProfile "HE 200 A" 200 190 0.00538 0.0003886 3.692e-05 mysteel),
            (23, DBSteelProfile "HE 200 B" 200 200 0.00781 0.0005696 5.696e-05 mysteel),
            (24, DBSteelProfile "HE 200 M" 206 220 0.01313 0.0009674 0.0001064 mysteel),
            (25, DBSteelProfile "HE 220 AA" 220 205 0.00515 0.0004069 4.17e-05 mysteel),
            (26, DBSteelProfile "HE 220 A" 220 210 0.00643 0.0005152 5.41e-05 mysteel),
            (27, DBSteelProfile "HE 220 B" 220 220 0.0091 0.0007355 8.091e-05 mysteel),
            (28, DBSteelProfile "HE 220 M" 226 240 0.01494 0.001217 0.000146 mysteel),
            (29, DBSteelProfile "HE 240 AA" 240 224 0.00604 0.000521 5.835e-05 mysteel),
            (30, DBSteelProfile "HE 240 A" 240 230 0.00768 0.0006751 7.763e-05 mysteel),
            (31, DBSteelProfile "HE 240 B" 240 240 0.0106 0.0009383 0.0001126 mysteel),
            (32, DBSteelProfile "HE 240 M" 248 270 0.01996 0.001799 0.0002429 mysteel),
            (33, DBSteelProfile "HE 260 AA" 260 244 0.0069 0.0006541 7.981e-05 mysteel),
            (34, DBSteelProfile "HE 260 A" 260 250 0.00868 0.0008364 0.0001045 mysteel),
            (35, DBSteelProfile "HE 260 B" 260 260 0.01184 0.001148 0.0001492 mysteel),
            (36, DBSteelProfile "HE 260 M" 268 290 0.02196 0.002159 0.0003131 mysteel),
            (37, DBSteelProfile "HE 280 AA" 280 264 0.0078 0.0007998 0.0001056 mysteel),
            (38, DBSteelProfile "HE 280 A" 280 270 0.00973 0.001013 0.0001367 mysteel),
            (39, DBSteelProfile "HE 280 B" 280 280 0.01314 0.001376 0.0001927 mysteel),
            (40, DBSteelProfile "HE 280 M" 288 310 0.02402 0.002551 0.0003955 mysteel),
            (41, DBSteelProfile "HE 300 AA" 300 283 0.00889 0.0007956 0.000138 mysteel),
            (42, DBSteelProfile "HE 300 A" 300 290 0.01125 0.00126 0.0001826 mysteel),
            (43, DBSteelProfile "HE 300 B" 300 300 0.01491 0.001678 0.0002517 mysteel),
            (44, DBSteelProfile "HE 300 M" 310 340 0.03031 0.003482 0.000592 mysteel),
            (45, DBSteelProfile "HE 320 AA" 300 301 0.00946 0.001093 0.0001645 mysteel),
            (46, DBSteelProfile "HE 320 A" 300 310 0.01244 0.001479 0.0002293 mysteel),
            (47, DBSteelProfile "HE 320 B" 300 320 0.01613 0.001926 0.0003082 mysteel),
            (48, DBSteelProfile "HE 320 M" 309 359 0.0312 0.003796 0.0006813 mysteel),
            (49, DBSteelProfile "HE 340 AA" 300 320 0.01005 0.001222 0.0001955 mysteel),
            (50, DBSteelProfile "HE 340 A" 300 330 0.01335 0.001678 0.0002769 mysteel),
            (51, DBSteelProfile "HE 340 B" 300 340 0.01709 0.002156 0.0003666 mysteel),
            (52, DBSteelProfile "HE 340 M" 309 377 0.03158 0.004052 0.0007637 mysteel),
            (53, DBSteelProfile "HE 360 AA" 300 339 0.01066 0.001359 0.0002304 mysteel),
            (54, DBSteelProfile "HE 360 A" 300 350 0.01428 0.001891 0.0003309 mysteel),
            (55, DBSteelProfile "HE 360 B" 300 360 0.01806 0.0024 0.0004319 mysteel),
            (56, DBSteelProfile "HE 360 M" 308 395 0.03188 0.004297 0.0008487 mysteel),
            (57, DBSteelProfile "HE 400 AA" 300 378 0.01177 0.001654 0.0003125 mysteel),
            (58, DBSteelProfile "HE 400 A" 300 390 0.0159 0.002311 0.0004507 mysteel),
            (59, DBSteelProfile "HE 400 B" 300 400 0.01978 0.002884 0.0005768 mysteel),
            (60, DBSteelProfile "HE 400 M" 307 432 0.03258 0.00482 0.001041 mysteel),
            (61, DBSteelProfile "HE 450 AA" 300 425 0.01271 0.001971 0.0004189 mysteel),
            (62, DBSteelProfile "HE 450 A" 300 440 0.0178 0.002896 0.0006372 mysteel),
            (63, DBSteelProfile "HE 450 B" 300 450 0.0218 0.003551 0.0007989 mysteel),
            (64, DBSteelProfile "HE 450 M" 307 478 0.03354 0.005501 0.001315 mysteel),
            (65, DBSteelProfile "HE 500 AA" 300 472 0.01369 0.002315 0.0005464 mysteel),
            (66, DBSteelProfile "HE 500 A" 300 490 0.01975 0.00355 0.0008697 mysteel),
            (67, DBSteelProfile "HE 500 B" 300 500 0.02386 0.004287 0.001072 mysteel),
            (68, DBSteelProfile "HE 500 M" 306 524 0.03443 0.00618 0.001619 mysteel),
            (69, DBSteelProfile "HE 550 AA" 300 522 0.01528 0.002792 0.0007287 mysteel),
            (70, DBSteelProfile "HE 550 A" 300 540 0.02118 0.004146 0.001119 mysteel),
            (71, DBSteelProfile "HE 550 B" 300 550 0.02541 0.004971 0.001367 mysteel),
            (72, DBSteelProfile "HE 550 M" 306 572 0.03544 0.006923 0.00198 mysteel),
            (73, DBSteelProfile "HE 600 AA" 300 571 0.01641 0.003218 0.000919 mysteel),
            (74, DBSteelProfile "HE 600 A" 300 590 0.02265 0.004787 0.001412 mysteel),
            (75, DBSteelProfile "HE 600 B" 300 600 0.027 0.005701 0.00171 mysteel),
            (76, DBSteelProfile "HE 600 M" 305 620 0.03637 0.00766 0.002374 mysteel),
            (77, DBSteelProfile "HE 600x337" 310 632 0.04292 0.008961 0.002832 mysteel),
            (78, DBSteelProfile "HE 600x399" 315 648 0.05085 0.01064 0.003446 mysteel),
            (79, DBSteelProfile "HE 650 AA" 300 620 0.01758 0.003676 0.001139 mysteel),
            (80, DBSteelProfile "HE 650 A" 300 640 0.02416 0.005474 0.001752 mysteel),
            (81, DBSteelProfile "HE 650 B" 300 650 0.02863 0.00648 0.002106 mysteel),
            (82, DBSteelProfile "HE 650 M" 305 668 0.03737 0.008433 0.002817 mysteel),
            (83, DBSteelProfile "HE 650x343" 309 680 0.04375 0.009815 0.000337 mysteel),
            (84, DBSteelProfile "HE 650x407" 314 696 0.05188 0.01165 0.004054 mysteel),
            (85, DBSteelProfile "HE 700 AA" 300 670 0.01909 0.00426 0.001427 mysteel),
            (86, DBSteelProfile "HE 700 A" 300 690 0.02605 0.006241 0.002153 mysteel),
            (87, DBSteelProfile "HE 700 B" 300 700 0.03064 0.00734 0.002569 mysteel),
            (88, DBSteelProfile "HE 700 M" 304 716 0.0383 0.009198 0.003293 mysteel),
            (89, DBSteelProfile "HE 700x352" 308 728 0.04486 0.01071 0.003897 mysteel),
            (90, DBSteelProfile "HE 700x418" 313 744 0.05319 0.0127 0.004725 mysteel),
            (91, DBSteelProfile "HE 800 AA" 300 770 0.02185 0.005426 0.002089 mysteel),
            (92, DBSteelProfile "HE 800 A" 300 790 0.02858 0.007682 0.003034 mysteel),
            (93, DBSteelProfile "HE 800 B" 300 800 0.03342 0.008977 0.003591 mysteel),
            (94, DBSteelProfile "HE 800 M" 303 814 0.04043 0.01087 0.004426 mysteel),
            (95, DBSteelProfile "HE 800x373" 308 826 0.04746 0.01269 0.005239 mysteel),
            (96, DBSteelProfile "HE 800x444" 313 842 0.0566 0.01507 0.006345 mysteel),
            (97, DBSteelProfile "HE 900 AA" 300 870 0.02522 0.006923 0.003011 mysteel),
            (98, DBSteelProfile "HE 900 A" 300 890 0.03205 0.009485 0.004221 mysteel),
            (99, DBSteelProfile "HE 900 B" 300 900 0.03713 0.01098 0.004941 mysteel),
            (100, DBSteelProfile "HE 900 M" 302 910 0.04236 0.01254 0.005704 mysteel),
            (101, DBSteelProfile "HE 900x391" 307 922 0.04977 0.01463 0.006743 mysteel),
            (102, DBSteelProfile "HE 900x466" 312 938 0.05937 0.01738 0.008149 mysteel),
            (103, DBSteelProfile "HE 1000 AA" 300 970 0.02822 0.00838 0.004065 mysteel),
            (104, DBSteelProfile "HE 1000x249" 300 980 0.03168 0.009818 0.004811 mysteel),
            (105, DBSteelProfile "HE 1000 A" 300 990 0.03468 0.01119 0.005538 mysteel),
            (106, DBSteelProfile "HE 1000 B" 300 1000 0.04 0.01289 0.006447 mysteel),
            (107, DBSteelProfile "HE 1000 M" 302 1008 0.04442 0.01433 0.007223 mysteel),
            (108, DBSteelProfile "HE 1000x393" 303 1016 0.05002 0.0159 0.008077 mysteel),
            (109, DBSteelProfile "HE 1000x415" 304 1020 0.05287 0.016728 0.008531 mysteel),
            (110, DBSteelProfile "HE 1000x438" 305 1026 0.05572 0.01774 0.009098 mysteel),
            (111, DBSteelProfile "HE 1000x494" 309 1036 0.06291 0.019845 0.01028 mysteel),
            (112, DBSteelProfile "HE 1000x584" 314 1056 0.07437 0.0236 0.012461 mysteel),
            (113, DBSteelProfile "IPE 300" 150 300 0.00538 0.0005571 0.00008356 mysteel)
            ]
    where mysteel = M.newSteel "S355"
#endif
