

module Compiler
( compile 
, testIt
) where
  
import Data
import Data.Bits
import Test.HUnit
import Data.Word

type VType = Word64

basis = 0xFFFFFFFFffffffff :: VType

compile :: Prog -> (VType -> VType)
compile (Prog e) = compileExp e

compileExp :: Expr -> (VType -> VType)
compileExp (P Zero)  = (\x -> 0)
compileExp (P One)   = (\x -> 1)
compileExp (P X)     = (\x -> x)
compileExp (Op1 o e) = op1 o $ compileExp e
compileExp (Op2 o e1 e2) = (\x -> op2 o (compileExp e1 x) (compileExp e2 x))

op1 :: Op1 -> (VType -> VType) -> (VType -> VType)
op1 Not f = xor basis . f
op1 Shl1 f = (flip shiftL) 1 . f
op1 Shr1 f = (flip shiftR) 1 . f
op1 Shr4 f = (flip shiftR) 4 . f
op1 Shr16 f = (flip shiftR) 16 . f

op2 And f1 f2 = (.&.) f1 f2
op2 Or f1 f2 = (.|.) f1 f2
op2 Xor f1 f2 = xor f1 f2
op2 Plus f1 f2 = (+) f1 f2

-- Tests

testShl1 = TestCase (assertEqual "op1 Shl1"
                     [0, 2, 4, 6, 0x1fe, 0xFFFFFFFFfffffffe]
                     $ map (op1 Shl1 (id))
                     [0, 1, 2, 3, 0xff, 0xFFFFFFFFffffffff])
        
tests = TestList [TestLabel "testShl1" testShl1
                 ]

testIt = runTestTT tests