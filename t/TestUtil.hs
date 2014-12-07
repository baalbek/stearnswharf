{-# LANGUAGE OverloadedStrings #-}

module TestUtil where 

import Test.HUnit (test,assertEqual,runTestTT,(~:))
import Text.XML.Light (parseXMLDoc,Element)

import StearnsWharf.Common (ro2dec)
import StearnsWharf.Output (NodeResult,BeamResult,moment,shear,xTrans,yTrans,nr1,nr2,brId,printResults)
import StearnsWharf.System (stearnsWharfResult)
import StearnsWharf.XML.XmlNodes (createMatstatNodes)
import StearnsWharf.XML.XmlLoads (createLoads,createPointLoads,LoadMap)
import StearnsWharf.Loads (PointLoad)
import qualified StearnsWharf.Nodes as N

assertAtNode :: BeamResult -> (BeamResult -> NodeResult) -> (NodeResult -> Double) -> String -> Double -> Int -> IO ()
assertAtNode beamRes nodeFun valueFun msg expected n = do
    assertEqual msgWithBeamId expected (ro2dec (valueFun $ nodeFun beamRes) n)
        where msgWithBeamId = "[" ++ (brId beamRes) ++ "] " ++ msg

assertMomentAtNode1 beamResult expected n = assertAtNode beamResult nr1 moment "Moment node 1" expected n 

assertMomentAtNode2 beamResult expected n = assertAtNode beamResult nr2 moment "Moment node 2" expected n 

assertDeflAtNode1 beamResult expected n = assertAtNode beamResult nr1 ((*1000.0) . yTrans) "Deflection node 1" expected n 

assertDeflAtNode2 beamResult expected n = assertAtNode beamResult nr2 ((*1000.0) . yTrans) "Deflection node 2" expected n 

assertShearAtNode1 beamResult expected n = assertAtNode beamResult nr1 shear "Shear node 1" expected n 

assertShearAtNode2 beamResult expected n = assertAtNode beamResult nr2 shear "Shear node 2" expected n 

myFileName :: String -> String
myFileName testCase = "/home/rcs/opt/haskell/stearnswharf/t/" ++ testCase ++ ".xml"


myDoc :: String -> IO Element
myDoc tc = do
    let fname = myFileName tc 
    s <- (readFile fname)
    let Just doc = parseXMLDoc s 
    return doc

myTestResult :: String -> IO [BeamResult]
myTestResult tc = do 
    doc <- myDoc tc
    return $ stearnsWharfResult doc

myNodes :: String -> IO N.NodeMap
myNodes tc = do
    doc <- myDoc tc
    return $ createMatstatNodes doc

myDistLoads :: String -> IO LoadMap
myDistLoads tc = do
    doc <- myDoc tc
    return $ createLoads doc

myPointLoads :: String -> IO [PointLoad]
myPointLoads tc = do
    doc <- myDoc tc
    let nm = createMatstatNodes doc
    return $ createPointLoads nm doc
