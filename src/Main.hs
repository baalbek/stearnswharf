{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
-- #define RCS_DEMO

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import StearnsWharf.System (runStearnsWharf)

data Main = Main { 
        h :: String,
        db :: String,
        u :: String,
        s :: Int,
        lc :: Int
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            h      %> [ Group "Database", Help "Database host", Default "xochitecatl" ] ,
            db     %> [ Group "Database", Help "Database name", Default "engineer" ] ,
            u      %> [ Group "Database", Help "Database user", Default "engineer" ] ,
            s      %> [ Group "System", Positional 0, Required True ] ,
            lc     %> [ Group "Load", Help "Load case", ArgHelp "LOADCASE", Default (1 :: Int) ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "Stearns Wharf Structural Matrix Analysis"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> 
    -- putStrLn (printf "host=%s, dbname=%s, user=%s, sys.id=%d, load case=%d" (h opts) (db opts) (u opts) (s opts) (lc opts)) >>
    runStearnsWharf (h opts) (db opts) (u opts) (s opts) (lc opts) >>
    return ()
