

module GenTests
( testIt ) where

import qualified Gen as G
import Data
import Test.HUnit

-- Tests

test1 = TestCase (assertEqual "gen 3 [not]"
      [(Op1 Not (P Open))]
      (G.gen (3-1) $ readOps ["not"]))

test2 = TestCase (assertEqual "gen 4 [and]"
      [(Op2 And (P Open) (P Open))]
      (G.gen (4-1) $ readOps ["and"]))

test3 = TestCase (assertEqual "gen 4 [and, not]"
      [(Op1 Not (Op1 Not (P Open))),(Op2 And (P Open) (P Open))]
      (G.gen (4-1) $ readOps ["and", "not"]))

test4 = TestCase (assertEqual "gen 6 [tfold]"
      [(TFold (P' (Param Open)))]
      (G.gen (6-1) $ readOps ["tfold"]))

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ]

testIt = do runTestTT tests
            G.testIt
