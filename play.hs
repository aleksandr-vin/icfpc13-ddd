

import Data
import Compiler
import Test.HUnit
import Numeric
import Data.Char
import Data.Word

-- Tests

test1 = TestCase (assertEqual ""
                  [0,0,0]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (P Zero))
              
test2 = TestCase (assertEqual ""
                  [1,1,1]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (P One))
              
test3 = TestCase (assertEqual ""
                  [0,1,2]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (P X))
              
test4 = TestCase (assertEqual ""
                  [0xFFFFFFFFffffffff,0xFFFFFFFFffffffff,0xFFFFFFFFffffffff]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (Op1 Not (P Zero)))
        
test5 = TestCase (assertEqual ""
                  [0xFFFFFFFFfffffffe,0xFFFFFFFFfffffffe,0xFFFFFFFFfffffffe]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (Op1 Not (P One)))
        
test6 = TestCase (assertEqual ""
                  [0xFFFFFFFFffffffff,0xFFFFFFFFfffffffe,0xFFFFFFFFfffffffd]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (Op1 Not (P X)))
        
test7 = TestCase (assertEqual ""
                  [0,1,2]
                  $ map (foo)
                  [0,1,2])
  where foo = compile (Prog (Op1 Not (Op1 Not (P X))))
        
test8 = TestCase (assertEqual ""
                  [0,2,4,0x1fe,0xFFFFFFFFfffffffe]
                  $ map (foo)
                  [0,1,2,0xff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shl1 (P X)))
        
test9 = TestCase (assertEqual ""
                  ([0,4,8,0x3fc,0xFFFFFFFFfffffffc] :: [Data.Word.Word64])
                  $ map (foo)
                  [0,1,2,0xff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shl1 (Op1 Shl1 (P X))))
        
test10 = TestCase (assertEqual ""
                  [0,0,1,2,0x7f,0x7FFFFFFFffffffff]
                  $ map (foo)
                  [0,1,2,4,0xff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shr1 (P X)))


tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ,TestLabel "test5" test5
                 ,TestLabel "test6" test6
                 ,TestLabel "test7" test7
                 ,TestLabel "test8" test8
                 ,TestLabel "test9" test9
                 ,TestLabel "test10" test10
                 ]

play = do testIt
          runTestTT tests

-- Utils

hex n = ("0x"++) $ showIntAtBase 16 intToDigit n ""