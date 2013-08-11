

module OpenerTests
( testIt ) where

import Data
import Opener
import Test.HUnit
import Data.List

-- Tests

test1 = TestCase (assertEqual "open [(Op1 Not (P Open))]"
                  [(Op1 Not (P Zero))
                  ,(Op1 Not (P One))
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
                  $ open [(Op2 And (P Open) (P Open))])
        
testAnd = TestCase (assertEqual "open [(Op2 And (Op1 Not (P X)) (Op1 Not (P X)))]"
                  [(Op1 Not (P X))]
                  $ open [(Op2 And (Op1 Not (P X)) (Op1 Not (P X)))])
        
testAnd' = TestCase (assertEqual "open [(TFold (Op2' And (Op1' Not (P' Y)) (Op1' Not (P' Y))))]"
                  [(TFold (Op1' Not (P' Y)))]
                  $ open [(TFold (Op2' And (Op1' Not (P' Y)) (Op1' Not (P' Y))))])
        
testOr = TestCase (assertEqual "open [(Op2 Or (Op1 Not (P X)) (Op1 Not (P X)))]"
                  [(Op1 Not (P X))]
                  $ open [(Op2 Or (Op1 Not (P X)) (Op1 Not (P X)))])
        
testOr' = TestCase (assertEqual "open [(Op2 Or (Op1 Not (P X)) (Op1 Not (P X)))]"
                  [(TFold (Op1' Not (P' Y)))]
                  $ open [(TFold (Op2' Or (Op1' Not (P' Y)) (Op1' Not (P' Y))))])        

testAnd1 = TestCase (assertEqual "open [(Op2 And (P One) (Op1 Not (P X)))]"
                  [(P Zero)]
                  $ open [(Op2 And (P Zero) (Op1 Not (P X)))])
        
testAnd'1 = TestCase (assertEqual "open [(Op2 And (Op1 Not (P X)) (Op1 Not (P X)))]"
                  [(P Zero)]
                  $ open [(TFold (Op2' And (P' (Param Zero)) (Op1' Not (P' Y))))])
            
testOr1 = TestCase (assertEqual "open [(Op2 Or (P One) (Op1 Not (P X)))]"
                  [(Op2 Or (P One) (Op1 Not (P X)))]
                  $ open [(Op2 Or (P One) (Op1 Not (P X)))])
        
testOr'1 = TestCase (assertEqual "open [(Op2 Or (Op1 Not (P X)) (Op1 Not (P X)))]"
                  [(TFold (Op2' Or (P' (Param One)) (Op1' Not (P' Y))))]
                  $ open [(TFold (Op2' Or (P' (Param One)) (Op1' Not (P' Y))))])            

testPlus = TestCase (assertEqual "open [..(Op2 Plus)..]"
                  (sort $ nub
                   [(P X)
                   ,(Op1 Shl1 (P One))
                   ,(Op1 Shl1 (P X))
                   ,(Op1 Shl1 (Op1 Not (P X)))
                   ,(Op2 Plus (P X) (Op1 Not (P X)))
                   ])
                  $ sort $ open
                  [(Op2 Plus (P Zero) (P X))
                  ,(Op2 Plus (P One) (P One))
                  ,(Op2 Plus (P X) (P X))
                  ,(Op2 Plus (Op1 Not (P X)) (Op1 Not (P X)))
                  ,(Op2 Plus (P X) (Op1 Not (P X)))
                  ])

testPlus' = TestCase (assertEqual "open [..(Op2' Plus)..] part1"
                  (sort $ nub
                   [(TFold (Op1' Shl1 (P' (Param One))))
                   ,(TFold (Op1' Shl1 (P' Y)))
                   ])
                  $ sort $ open
                  [(TFold (Op2' Plus (P' (Param One)) (P' (Param One))))
                  ,(TFold (Op2' Plus (P' Y) (P' Y)))
                  ])

testPlus'1 = TestCase (assertEqual "open [..(Op2' Plus)..] part2"
                  (sort $ nub
                   [(TFold (P' Y))
                   ,(TFold (Op1' Not (P' (Param X))))
                   ])
                  $ sort $ open
                  [(TFold (Op2' Plus (P' (Param Zero)) (P' Y)))
                  ,(TFold (Op2' Plus (P' (Param Zero)) (Op1' Not (P' (Param X)))))
                  ])
            
testPlus'2 = TestCase (assertEqual "open [..(Op2' Plus)..] part3"
                  (sort $ nub
                   [(TFold (Op1' Shl1 (P' (Param X))))
                   ,(TFold (Op1' Shl1 (Op1' Not (P' Y))))
                   ,(TFold (Op2' Plus (P' Y) (Op1' Not (P' Y))))
                   ])
                  $ sort $ open
                  [(TFold (Op2' Plus (P' (Param X)) (P' (Param X))))
                  ,(TFold (Op2' Plus (Op1' Not (P' Y)) (Op1' Not (P' Y))))
                  ,(TFold (Op2' Plus (P' Y) (Op1' Not (P' Y))))
                  ])


tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "testAnd" testAnd
                 ,TestLabel "testAnd'" testAnd'
                 ,TestLabel "testOr" testOr
                 ,TestLabel "testOr'" testOr'
                 ,TestLabel "testAnd1" testAnd1
                 ,TestLabel "testAnd'1" testAnd'1
                 ,TestLabel "testOr1" testOr1
                 ,TestLabel "testOr'1" testOr'1
                 ,TestLabel "testPlus" testPlus
                 ,TestLabel "testPlus'" testPlus'
                 ,TestLabel "testPlus'1" testPlus'1
                 ,TestLabel "testPlus'2" testPlus'2
                 ]

testIt = runTestTT tests
