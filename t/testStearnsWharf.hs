{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit (runTestTT)
import TestComboLoads (testComboLoads)
import TestDistLoads (testDistLoads)
import TestPointLoads (testPointLoads)
import TestNodes (testNodes)
import TestLoads (testLoads)

main = do
    runTestTT testNodes
    runTestTT testLoads
    runTestTT testDistLoads
    runTestTT testPointLoads
    runTestTT testComboLoads
