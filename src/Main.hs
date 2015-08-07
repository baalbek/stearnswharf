{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

import StearnsWharf.CmdLine(cmd)
import StearnsWharf.System (runStearnsWharf)


main = cmdArgs cmd >>= \x -> 
    putStrLn (show x) >>
    runStearnsWharf x >>
    return ()

