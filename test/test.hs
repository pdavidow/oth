-- http://documentup.com/feuerbach/tasty

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


import Board ( board_DisplayString, initialBoard )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

-- invariant: flipCount for 4 corners == 0 

unitTests = testGroup "Unit tests"
  [ testCase "board_DisplayString initialBoard" $
    board_DisplayString initialBoard @?= "   A  B  C  D  E  F  G  H\n1  -  -  -  -  -  -  -  - \n2  -  -  -  -  -  -  -  - \n3  -  -  -  -  -  -  -  - \n4  -  -  -  o  x  -  -  - \n5  -  -  -  x  o  -  -  - \n6  -  -  -  -  -  -  -  - \n7  -  -  -  -  -  -  -  - \n8  -  -  -  -  -  -  -  - \n"
  ]