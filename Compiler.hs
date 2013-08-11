

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

op1 :: Op1 -> (VType -> VType) -> (VType -> VType)
op1 Not f = xor basis . f
op1 Shl1 f = (flip shiftL) 1 . f
op1 Shr1 f = (flip shiftR) 1 . f


-- Tests

testShl1 = TestCase (assertEqual "op1 Shl1"
                     [0, 2, 4, 6, 0x1fe, 0xFFFFFFFFfffffffe]
                     $ map (op1 Shl1 (id))
                     [0, 1, 2, 3, 0xff, 0xFFFFFFFFffffffff])
        
tests = TestList [TestLabel "testShl1" testShl1
                 ]

testIt = runTestTT tests