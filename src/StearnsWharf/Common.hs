module StearnsWharf.Common where

import Numeric.LinearAlgebra (Vector)
import Numeric.Container (vecdisp,dispf)

type BeamTypeId = String

type CalculatedResult = (Vector Double, Vector Double)

type StaticMoment = Double

type Shear = Double

type Load = Double

data MomentType = FieldMoment | SupportMoment

prnVec :: String -> Vector Double -> IO ()
prnVec msg v = do
    putStrLn $ msg ++ (vecdisp (dispf 2) v)
    --print $ v @> 0
    --
    --

radians :: Double -> Double
radians d = d * pi / 180.0

ro2dec :: Double -> Int -> Double
ro2dec v n = (fromInteger $ round $ v * (10^n))/(10^n)

nth :: Int -> [a] -> a
nth n = head . drop n

-- | Second Area Moment for a rectangular profile
-- b : profilbredde
-- h : profilhøyde
samr :: Double -> Double -> Double
samr b h = b * h**3 / 12.0

-- | First Area Moment for a rectangular profile
famr :: Double -> Double -> Double
famr b h = b * h**2 / 6.0


