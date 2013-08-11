

module GenTests
( testIt ) where

import qualified Gen as G
import Data
import Test.HUnit
import Data.List

-- Tests

test1 = TestCase (assertEqual "gen 3 [not]"
      [(Op1 Not (P Open))]
      (G.gen (3-1) $ readOps ["not"]))

test2 = TestCase (assertEqual "gen 4 [and]"
      [(Op2 And (P Open) (P Open))]
      (G.gen (4-1) $ readOps ["and"]))

test3 = TestCase (assertEqual "gen 5 [and, not]"
      (sort [(Op2 And (P Open) (Op1 Not (P Open)))
            ,(Op1 Not (Op2 And (P Open) (P Open)))
      ])
      (sort . G.gen (5-1) $ readOps ["and", "not"]))

test4 = TestCase (assertEqual "gen 6 [tfold]"
      [(TFold (P' (Param Open)))]
      (G.gen (6-1) $ readOps ["tfold"]))

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ]

testIt = do G.testIt
            runTestTT tests
            
