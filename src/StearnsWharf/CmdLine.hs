{-# LANGUAGE DeriveDataTypeable #-}
module StearnsWharf.CmdLine where

import System.Console.CmdArgs (Data,Typeable,typ,def,groupname,(&=))

data CmdLine = 
    CmdLine {
        host :: String,
        dbname :: String,
        user :: String,
        system :: Int,
        loadcase :: Int } deriving (Show, Data, Typeable)

cmdLine = CmdLine {
        host = "xochitecatl" &= groupname "Database",
        dbname = "engineer" &= groupname "Database",
        user = "engineer" &= groupname "Database",
        system = 0 &= groupname "System",
        loadcase = 1 &= groupname "System"}


        
        

