

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
        
test5 = TestCase (assertEqual "gen 5 [if0]"
      [(If0 (P Open) (P Open) (P Open))]
      (G.gen (5-1) $ readOps ["if0"]))
        
test6 = TestCase (assertEqual "gen 6 [if0, not]"
                  (sort $ [(If0 (Op1 Not (P Open)) (P Open) (P Open))
                          ,(If0 (P Open) (Op1 Not (P Open)) (P Open))
                          ,(If0 (P Open) (P Open) (Op1 Not (P Open)))
                          ,(Op1 Not (If0 (P Open) (P Open) (P Open)))
                          ])
                  (sort . G.gen (6-1) $ readOps ["if0", "not"]))
        
test7 = TestCase (assertEqual "gen 9 [if0, tfold]"
                  (sort $ [(TFold (If0' (P' (Param Open)) (P' (Param Open)) (P' (Param Open))))
                          ,(If0 (TFold (P' (Param Open))) (P Open) (P Open))
                          ,(If0 (P Open) (TFold (P' (Param Open))) (P Open))
                          ,(If0 (P Open) (P Open) (TFold (P' (Param Open))))
                          ])
                  (sort $ G.gen (9-1) $ readOps ["if0", "tfold"]))
        
test8 = TestCase (assertEqual "gen 6 [fold]"
                  [(Fold (P Open) (P Open) (P' (Param Open)))]
                  (G.gen (6-1) $ readOps ["fold"]))

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ,TestLabel "test5" test5
                 ,TestLabel "test6" test6
                 ,TestLabel "test7" test7
                 ,TestLabel "test8" test8
                 ]

testIt = do G.testIt
            runTestTT tests
            
