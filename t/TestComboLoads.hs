{-# LANGUAGE OverloadedStrings #-}

module TestComboLoads where 

import Test.HUnit (test,assertEqual,runTestTT,(~:))
import Text.XML.Light (parseXMLDoc,Element)

import StearnsWharf.Common (nth)
import StearnsWharf.Output (printResults)
import TestUtil (myTestResult,
                 assertMomentAtNode1,
                 assertMomentAtNode2,
                 assertShearAtNode1,
                 assertShearAtNode2,
                 assertDeflAtNode1,
                 assertDeflAtNode2)

testComboLoads = test [
                "testComboLoad01" ~: do 
                    result <- myTestResult "comboload01"
                    --mapM_ printResults result
                    let a = nth 1 result
                    assertDeflAtNode2 a (-179.2) 1
                ]
