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
        host = "192.168.56.63" &= groupname "Database",
        dbname = "engineer" &= groupname "Database",
        user = "engineer" &= groupname "Database",
        system = 15 &= groupname "System",
        loadcase = 1 &= groupname "System"}


        
        

