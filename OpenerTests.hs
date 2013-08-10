

module OpenerTests
( testIt ) where

import Data
import Opener
import Test.HUnit
import Data.List

-- Tests

test1 = TestCase (assertEqual "open [(Op1 Not (P Open))]"
                  [(P One) -- (Op1 Not (P Zero))
                  ,(P Zero) -- (Op1 Not (P One))
                  ,(Op1 Not (P X))]
                  $ open [(Op1 Not (P Open))])

test2 = TestCase (assertEqual "open [(Op2 And (P Open) (P Open))]"
                  (nub
                   [(P Zero) -- (Op2 And (P Zero) (P Zero))
                   ,(P Zero) -- (Op2 And (P Zero) (P One))
                   ,(P Zero) -- (Op2 And (P Zero) (P X))
                   ,(P Zero) -- (Op2 And (P One) (P Zero))
                   ,(P One)  -- (Op2 And (P One) (P One))
                   ,(Op2 And (P One) (P X))
                   ,(P Zero) -- (Op2 And (P X) (P Zero))
                   ,(Op2 And (P One) (P X)) -- (Op2 And (P X) (P One))
                   ,(P X)    -- (Op2 And (P X) (P X))
                   ])
                  $ nub $ open [(Op2 And (P Open) (P Open))])

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ]

testIt = runTestTT tests
