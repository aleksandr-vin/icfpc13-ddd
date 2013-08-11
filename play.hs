

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
        
test11 = TestCase (assertEqual ""
                  [0,1,2,4,0xff,0x7FFFFFFFffffffff]
                  $ map (foo)
                  [0,1,2,4,0xff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shr1 (Op1 Shl1 (P X))))
        
test12 = TestCase (assertEqual ""
                  [0,0,0,0,0x00,0x0f,0x0FFFFFFFffffffff]
                  $ map (foo)
                  [0,1,2,4,0x08,0xff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shr4 (P X)))
        
test13 = TestCase (assertEqual ""
                  [0,0,0,0,0x00,0x00,0x0000,0x00001,0x0000a0,0x0000FFFFffffffff]
                  $ map (foo)
                  [0,1,2,4,0x08,0xff,0xffff,0x10000,0xa00000,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op1 Shr16 (P X)))

test14 = TestCase (assertEqual ""
                  [0,0,0,0]
                  $ map (foo)
                  [0,1,2,4])
  where foo = compile (Prog (Op2 And (P X) (P Zero)))
        
test15 = TestCase (assertEqual ""
                  [0,1,0,1,0,1,0]
                  $ map (foo)
                  [0,1,2,3,4,5,6])
  where foo = compile (Prog (Op2 And (P X) (P One)))
        
test16 = TestCase (assertEqual ""
                  [0,1,2,4]
                  $ map (foo)
                  [0,1,2,4])
  where foo = compile (Prog (Op2 Or (P X) (P Zero)))
        
test17 = TestCase (assertEqual ""
                  [1,1,3,3,5,5,7]
                  $ map (foo)
                  [0,1,2,3,4,5,6])
  where foo = compile (Prog (Op2 Or (P X) (P One)))
        
test18 = TestCase (assertEqual ""
                   [0,1,2,4,0xff,0xff0ff0,0xFFFFFFFFffffffff]
                   $ map (foo)
                   [0,1,2,4,0xff,0xff0ff0,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Xor (P X) (P Zero)))
        
test19 = TestCase (assertEqual ""
                   [1,0,3,5,0xfe,0xff0ff1,0xFFFFFFFFfffffffe]
                   $ map (foo)
                   [0,1,2,4,0xff,0xff0ff0,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Xor (P X) (P One)))
        
test20 = TestCase (assertEqual ""
                   [0,0,0,0,0x00,0x000000,0x0000000000000000]
                   $ map (foo)
                   [0,1,2,4,0xff,0xff0ff0,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Xor (P X) (P X)))
        
test21 = TestCase (assertEqual ""
                   [0xFFFFFFFFffffffff,0xFFFFFFFFffffffff,0xFFFFFFFFffffffff
                   ,0xFFFFFFFFffffffff,0xFFFFFFFFffffffff,0xFFFFFFFFffffffff
                   ,0xFFFFFFFFffffffff]
                   $ map (foo)
                   [0,1,2
                   ,4,0xff,0xff0ff0
                   ,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Xor (P X) (Op1 Not (P X))))
        
test22 = TestCase (assertEqual ""
                  [1,2,3,4,5,6,7,0x100,0x0000000000000000]
                  $ map (foo)
                  [0,1,2,3,4,5,6,0x0ff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Plus (P X) (P One)))
        
test23 = TestCase (assertEqual ""
                  [0,2,4,6,8,0xa,0xc,0x1fe,0xFFFFFFFFfffffffe]
                  $ map (foo)
                  [0,1,2,3,4,0x5,0x6,0x0ff,0xFFFFFFFFffffffff])
  where foo = compile (Prog (Op2 Plus (P X) (P X)))


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
                 ,TestLabel "test11" test11
                 ,TestLabel "test12" test12
                 ,TestLabel "test13" test13
                 ,TestLabel "test14" test14
                 ,TestLabel "test15" test15
                 ,TestLabel "test16" test16
                 ,TestLabel "test17" test17
                 ,TestLabel "test18" test18
                 ,TestLabel "test19" test19
                 ,TestLabel "test20" test20
                 ,TestLabel "test21" test21
                 ,TestLabel "test22" test22
                 ,TestLabel "test23" test23
                 ]

play = do testIt
          runTestTT tests

-- Utils

hex n = ("0x"++) $ showIntAtBase 16 intToDigit n ""
