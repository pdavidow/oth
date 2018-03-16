-- http://documentup.com/feuerbach/tasty

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


import Board ( board_DisplayString, initialBoard )
import Position ( raysFrom )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

-- invariant: flipCount for 4 corners == 0 

unitTests = testGroup "Unit tests"
  [ testCase "board_DisplayString initialBoard" $
    board_DisplayString initialBoard @?= "   A  B  C  D  E  F  G  H\n1  -  -  -  -  -  -  -  - \n2  -  -  -  -  -  -  -  - \n3  -  -  -  -  -  -  -  - \n4  -  -  -  o  x  -  -  - \n5  -  -  -  x  o  -  -  - \n6  -  -  -  -  -  -  -  - \n7  -  -  -  -  -  -  -  - \n8  -  -  -  -  -  -  -  - \n"
  
  , testCase "raysFrom" $
    raysFrom (5,6)  @?= [[(5,6),(4,6),(3,6),(2,6),(1,6)],[(5,6),(6,6),(7,6),(8,6)],[(5,6),(5,7),(5,8)],[(5,6),(5,5),(5,4),(5,3),(5,2),(5,1)],[(5,6),(4,7),(3,8)],[(5,6),(4,5),(3,4),(2,3),(1,2)],[(5,6),(6,7),(7,8)],[(5,6),(6,5),(7,4),(8,3)]]
  ]