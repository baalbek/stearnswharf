{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

import StearnsWharf.CmdLine (cmdLine)
-- import StearnsWharf.System (runStearnsWharf)


main = cmdArgs cmdLine >>= \x -> 
    putStrLn (show x) >>
    -- runStearnsWharf x >>
    return ()

