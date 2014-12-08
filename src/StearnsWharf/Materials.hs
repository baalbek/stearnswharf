module StearnsWharf.Materials where

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
                          emodulus, mySigma, myTau :: Double } 
                | Concrete { 
                             emodulus :: Double } 
                deriving Show

data Stress = Stress { sigma, tau :: Double } 
                deriving Show

{-
newWood :: String -> Material 
newWood wpid = case wpid of "C16" -> Wood 8000 16 3.2  
                            "C22" -> Wood 10000 22 3.8
                            "C24" -> Wood 10000 24 4.0
                            "CEL40C" -> Glulam 14000 30 3.5
-}

newSteel :: String -> Material
newSteel wpid = case wpid of "S355" -> Steel 200000.0 355.0 ((/) 355.0 $ sqrt 2)
                             "S235" -> Steel 200000.0 235.0 ((/) 235.0 $ sqrt 2)
