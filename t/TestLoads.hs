{-# LANGUAGE OverloadedStrings #-}

module TestLoads where 

import Test.HUnit (test,assertEqual,(~:))
import Text.XML.Light (parseXMLDoc,Element)
import qualified Data.Map as Map

import StearnsWharf.Common (ro2dec)
import StearnsWharf.Loads (qy1,qy2,qx1,qx2,loadFactor)
import TestUtil (myDistLoads,myPointLoads)

testLoads = test [
                "testDistLoads" ~: do 
                    loads <- myDistLoads "loads"
                    let Just l1 = Map.lookup "l1" loads 
                    let Just l2 = Map.lookup "l2" loads 
                    let Just l3 = Map.lookup "l3" loads 
                    let Just l4 = Map.lookup "l4" loads 
                    let Just l5 = Map.lookup "l5" loads 
                    -- Load l1 <load id="l1" f="1.5" y="-10" />
                    assertEqual "[l1] qy1" (-10.0) (ro2dec ( qy1 l1) 1)
                    assertEqual "[l1] qy2" (-10.0) (ro2dec ( qy2 l1) 1)
                    assertEqual "[l1] qx1" 0.0 (ro2dec ( qx1 l1) 1)
                    assertEqual "[l1] qx2" 0.0 (ro2dec ( qx2 l1) 1)
                    assertEqual "[l1] load factor" 1.5 (ro2dec (loadFactor l1) 1)
                    -- Load l2 <load id="l2" f="1.2" y1="-10" y2="-5" />
                    assertEqual "[l2] qy1" (-10.0) (ro2dec ( qy1 l2) 1)
                    assertEqual "[l2] qy2" (-5.0) (ro2dec ( qy2 l2) 1)
                    assertEqual "[l2] qx1" 0.0 (ro2dec ( qx1 l2) 1)
                    assertEqual "[l2] qx2" 0.0 (ro2dec ( qx2 l2) 1)
                    assertEqual "[l2] load factor" 1.2 (ro2dec (loadFactor l2) 1)
                    -- Load l3 <load id="l3" f="1.2" x="-7" />
                    assertEqual "[l3] qy1" 0.0 (ro2dec ( qy1 l3) 1)
                    assertEqual "[l3] qy2" 0.0 (ro2dec ( qy2 l3) 1)
                    assertEqual "[l3] qx1" (-7.0) (ro2dec ( qx1 l3) 1)
                    assertEqual "[l3] qx2" (-7.0) (ro2dec ( qx2 l3) 1)
                    assertEqual "[l3] load factor" 1.2 (ro2dec (loadFactor l3) 1)
                    -- Load l4 <load id="l4" f="1.6" x1="-10" x2="-5" />
                    assertEqual "[l4] qy1" 0.0 (ro2dec (qy1 l4) 1)
                    assertEqual "[l4] qy2" 0.0 (ro2dec (qy2 l4) 1)
                    assertEqual "[l4] qx1" (-10.0) (ro2dec (qx1 l4) 1)
                    assertEqual "[l4] qx2" (-5.0) (ro2dec (qx2 l4) 1)
                    assertEqual "[l4] load factor" 1.6 (ro2dec (loadFactor l4) 1)
                    -- Load l5 <load id="l5" f="1.0" y1="-2.3" y2="5.4" x1="0.0" x2="-5.6" />
                    assertEqual "[l5] qy1" (-2.3) (ro2dec (qy1 l5) 1)
                    assertEqual "[l5] qy2" 5.4 (ro2dec (qy2 l5) 1)
                    assertEqual "[l5] qx1" 0.0 (ro2dec (qx1 l5) 1)
                    assertEqual "[l5] qx2" (-5.6) (ro2dec (qx2 l5) 1)
                    assertEqual "[l5] load factor" 1.0 (ro2dec (loadFactor l5) 1)
                ]
