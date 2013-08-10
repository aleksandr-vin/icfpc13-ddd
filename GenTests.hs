

module GenTests
( testIt ) where

import qualified Gen as G
import Data
import Test.HUnit

-- Tests

test1 = TestCase (assertEqual "gen 3 [not]"
      (Op1 Not (P Open))
      (G.gen 3 [read "not" :: Operations]))

test2 = TestCase (assertEqual "gen 5 [and]"
      (Op2 And (P Open) (P Open))
      (G.gen 5 [read "and" :: Operations]))

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ]

testIt = do runTestTT tests
            G.testIt
