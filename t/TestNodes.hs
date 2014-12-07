{-# LANGUAGE OverloadedStrings #-}

module TestNodes where 

import Test.HUnit (test,assertEqual,(~:))
import Text.XML.Light (parseXMLDoc,Element)
import qualified Data.Map as Map

import StearnsWharf.Common (ro2dec)
import StearnsWharf.Nodes (nx,ny,dof,globNdx,Dof(..))
import TestUtil (myNodes)

testNodes = test [
                "testNodes" ~: do 
                    nodes <- myNodes "nodes"
                    let Just n1 = Map.lookup "n1" nodes
                    let Just n2 = Map.lookup "n2" nodes
                    let Just n3 = Map.lookup "n3" nodes
                    assertEqual "[n1] x-coord" 0.0 (ro2dec (nx n1) 1)
                    assertEqual "[n1] y-coord" 0.0 (ro2dec (ny n1) 1)
                    assertEqual "[n2] x-coord" 2.5 (ro2dec (nx n2) 1)
                    assertEqual "[n2] y-coord" 1.0 (ro2dec (ny n2) 1)
                    assertEqual "[n3] x-coord" 5.0 (ro2dec (nx n3) 1)
                    assertEqual "[n3] y-coord" 2.0 (ro2dec (ny n3) 1)
                    assertEqual "[n1] dof" (Dof 0 0 1) (dof n1)
                    assertEqual "[n2] dof" (Dof 0 1 0) (dof n2)
                    assertEqual "[n3] dof" (Dof 0 1 1) (dof n3)
                ]
