{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

import StearnsWharf.CmdLine (cmdLine)
import StearnsWharf.System (runStearnsWharf,runStearnsWharfXML)

main :: IO ()
main = cmdArgs cmdLine >>= \x ->
    putStrLn (show x) >>
    runStearnsWharfXML x 
