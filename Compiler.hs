

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
compileExp (Op1 o e) = (\x -> op1 o (compileExp e x))
compileExp (Op2 o e1 e2) = (\x -> op2 o (compileExp e1 x) (compileExp e2 x))
compileExp (TFold e)     = (\x -> (tfold (compileExp' e x)) x)

--                          lambda (y        z        x)
compileExp' :: Expr'          -> (VType -> VType -> VType -> VType)
compileExp' (P' (Param Zero)) = (\y z x -> 0)
compileExp' (P' (Param One))  = (\y z x -> 1)
compileExp' (P' (Param X))    = (\y z x -> x)
compileExp' (P' Y)            = (\y z x -> x)
compileExp' (P' Z)            = (\y z x -> x)
compileExp' (Op1' o e)        = (\y z x -> op1 o (compileExp' e  y z x))
compileExp' (Op2' o e1 e2)    = (\y z x -> op2 o (compileExp' e1 y z x) (compileExp' e2 y z x))

op1 :: Op1 -> VType -> VType
op1 Not f = xor basis f
op1 Shl1 f = (flip shiftL) 1 f
op1 Shr1 f = (flip shiftR) 1 f
op1 Shr4 f = (flip shiftR) 4 f
op1 Shr16 f = (flip shiftR) 16 f

op2 :: Op2 -> VType -> VType -> VType
op2 And f1 f2 = (.&.) f1 f2
op2 Or f1 f2 = (.|.) f1 f2
op2 Xor f1 f2 = xor f1 f2
op2 Plus f1 f2 = (+) f1 f2

-- lambda  (x        y        z)  
tfold :: (VType -> VType -> VType) -> (VType -> VType)
tfold lambdaYZ = (\x -> x)

-- Tests

testShl1 = TestCase (assertEqual "op1 Shl1"
                     [0, 2, 4, 6, 0x1fe, 0xFFFFFFFFfffffffe]
                     $ map (op1 Shl1)
                     [0, 1, 2, 3, 0x0ff, 0xFFFFFFFFffffffff])
        
tests = TestList [TestLabel "testShl1" testShl1
                 ]

testIt = runTestTT tests