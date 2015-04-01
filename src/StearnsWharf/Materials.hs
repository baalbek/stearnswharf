module StearnsWharf.Materials where

import Data.Ratio (Ratio)

data Material = Wood { 
                       emodulus, 
                       mySigma, 
                       myTau :: Double,
                       glulam :: Bool,   -- ^ Limtre == True
                       stClass :: String -- ^ Strength Class
                } 
                | Glulam { 
                            emodulus, mySigma, myTau :: Double } 
                | Steel { 
                            emodulus2, mySigma2, myTau2 :: Ratio Integer } 
                | Concrete { 
                            emodulus :: Double } 
                deriving Show

data Stress = Stress { sigma, tau :: Double } 
                deriving Show

