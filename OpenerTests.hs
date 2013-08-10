

module OpenerTests
( testIt ) where

import Data
import Opener
import Test.HUnit

-- Tests

test1 = TestCase (assertEqual "open [(Op1 Not (P Open))]"
                  [(Op1 Not (P Zero)),(Op1 Not (P One)),(Op1 Not (P X))]
                  $open [(Op1 Not (P Open))])

test2 = TestCase (assertEqual "open [(Op2 And (P Open) (P Open))]"
                  [(Op2 And (P Zero) (P Zero))
                  ,(Op2 And (P Zero) (P One))
                  ,(Op2 And (P Zero) (P X))
--                  ,(Op2 And (P One) (P Zero))
                  ,(Op2 And (P One) (P One))
                  ,(Op2 And (P One) (P X))
--                  ,(Op2 And (P X) (P Zero))
--                  ,(Op2 And (P X) (P One))
                  ,(Op2 And (P X) (P X))
                  ]
                  $ open [(Op2 And (P Open) (P Open))])

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ]

testIt = runTestTT tests