{-# LANGUAGE OverloadedStrings #-}

module TestDistLoads where 

import Test.HUnit (test,assertEqual,runTestTT,(~:))
import Text.XML.Light (parseXMLDoc,Element)

import StearnsWharf.Output (printResults)
import TestUtil (myTestResult,assertDeflAtNode2)

testDistLoads = test [
                "testDistLoad01" ~: do 
                    result <- myTestResult "distload01"
                    -- mapM_ printResults result
                    assertDeflAtNode2 (head result) (-11.9) 1,
                "testDistLoad02" ~: do 
                    result <- myTestResult "distload02"
                    assertDeflAtNode2 (head result) (-12.0) 1,
                "testDistLoad03" ~: do 
                    result <- myTestResult "distload03"
                    assertDeflAtNode2 (head result) (-6.0) 1
                ]
