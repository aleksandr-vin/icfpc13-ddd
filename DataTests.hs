

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

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ]

testIt = runTestTT tests
