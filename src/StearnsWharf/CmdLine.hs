{-# LANGUAGE DeriveDataTypeable #-}
module StearnsWharf.CmdLine where

import System.Console.CmdArgs (Data,Typeable,typ,def,groupname,(&=))

data CmdLine =
    CmdLine {
        isxml :: Bool
        ,xmlfile :: Maybe String
        ,host :: String
        ,dbname :: String
        ,user :: String
        ,system :: Int
        ,loadcase :: Int } deriving (Show, Data, Typeable)

cmdLine = CmdLine {
        isxml = False &= groupname "Input/output"
        ,xmlfile = Just "/home/rcs/opt/haskell/stearnswharf/src/demo.xml" -- Nothing
        ,host = "172.17.0.2" &= groupname "Database"
        ,dbname = "engineer" &= groupname "Database"
        ,user = "engineer" &= groupname "Database"
        ,system = 15 &= groupname "System"
        ,loadcase = 1 &= groupname "System"}
