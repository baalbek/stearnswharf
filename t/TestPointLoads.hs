{-# LANGUAGE OverloadedStrings #-}

module TestPointLoads where 

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

testPointLoads = test [
                "testPointLoad01" ~: do 
                    result <- myTestResult "pointload01"
                    -- mapM_ printResults result
                    let a = head result
                    let b = last result
                    assertMomentAtNode1 a 0.0  1
                    assertMomentAtNode1 b 6.25 2
                    assertShearAtNode2 a (-2.5) 1
                    assertShearAtNode1 b (-2.5) 1
                    assertShearAtNode2 b 2.5 1
                    assertDeflAtNode2 a (-1.9) 1,
                "testPointLoad02" ~: do 
                    result <- myTestResult "pointload02"
                    assertDeflAtNode2 (head result) (-0.5) 1,
                "testPointLoad03" ~: do 
                    result <- myTestResult "pointload03"
                    assertDeflAtNode2 (head result) (-0.9) 1,
                "testPointLoad04" ~: do 
                    result <- myTestResult "pointload04"
                    let a = nth 1 result
                    assertDeflAtNode2 a (-83.8) 1,
                "testPointLoad05" ~: do 
                    result <- myTestResult "pointload05"
                    let a = nth 1 result
                    assertDeflAtNode2 a (-83.8) 1
                ]
