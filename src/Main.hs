{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import GHC.Float (float2Double)
import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import StearnsWharf.System (runStearnsWharf)

data Main = Main { 
        s :: Int,
        lc :: Int
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            s      %> [ Group "System", Positional 0, Required True ] ,
            lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default (1 :: Int) ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Stearns Wharf Structural Matrix Analysis"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> 
    runStearnsWharf (s opts) (lc opts) >>
    return ()
