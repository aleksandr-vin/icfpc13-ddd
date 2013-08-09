

module DataTests
( testIt ) where

import Data
import Test.HUnit

-- Tests

test1 = TestCase (assertEqual "read \"plus\""
      (OOp2 Plus) (read "plus" :: Operations))

test2 = TestCase (assertEqual "read \"not\""
      (OOp1 Not) (read "not" :: Operations))

test3 = TestCase (assertEqual "read \"if0\""
      OIf0 (read "if0" :: Operations))

testShow1 = TestCase (assertEqual "show 1+x"
      "(plus 1 x)" (show (Op2 Plus (P One) (P X))))

testShow2 = TestCase (assertEqual "show not zero"
      "(not 0)" (show (Op1 Not (P Zero))))

testShow3 = TestCase (assertEqual "show Z"
      "z" (show (P' Z)))

testShow4 = TestCase (assertEqual "show fold w shl1"
      "(fold x x (lambda (y z) (shl1 y)))" (show (Fold (P X) (P X) (Op1' Shl1 (P' Y)) )) )
{-
  show Open = "?"
  show Shr1 = "shr1"
  show Shr4 = "shr4"
  show Shr16 = "shr16"
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show (If0 a b c) = "(if0 "++show a++" "++show b++" "++show c++")"
  show (TFold a) = "(fold "++show X++" "++show Zero++" (lambda ("++show X++"y) ("++show a++"))"
-}
	  
tests = TestList [TestLabel "test1" test1
                  ,TestLabel "test2" test2
                  ,TestLabel "test3" test3
                  ,TestLabel "testShow1" testShow1
                  ,TestLabel "testShow2" testShow2
                  ,TestLabel "testShow3" testShow3
                  ,TestLabel "testShow4" testShow4
                 ]

testIt = runTestTT tests
