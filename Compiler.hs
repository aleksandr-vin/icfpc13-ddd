

module Compiler
( compile 
, testIt
, nByte
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
compileExp (TFold e)     = (\x -> tfold (compileExp' e) x)

--                          lambda (y        z        x)
compileExp' :: Expr'          -> (VType -> VType -> VType -> VType)
compileExp' (P' (Param Zero)) = (\y z x -> 0)
compileExp' (P' (Param One))  = (\y z x -> 1)
compileExp' (P' (Param X))    = (\y z x -> x)
compileExp' (P' Y)            = (\y z x -> y)
compileExp' (P' Z)            = (\y z x -> z)
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

-- (fold e1       e2   (lambda  (y        z        x)          ) )   (x)
fold :: VType -> VType     -> (VType -> VType -> VType -> VType) -> VType -> VType
fold e1 e2 lambdaYZ x = lambdaYZ y8 z8 x
  where z8 = lambdaYZ y7 z7 x
        z7 = lambdaYZ y6 z6 x
        z6 = lambdaYZ y5 z5 x
        z5 = lambdaYZ y4 z4 x
        z4 = lambdaYZ y3 z3 x
        z3 = lambdaYZ y2 z2 x
        z2 = lambdaYZ y1 z1 x
        z1 = e2
        y8 = nByte 8 e1
        y7 = nByte 7 e1
        y6 = nByte 6 e1
        y5 = nByte 5 e1
        y4 = nByte 4 e1
        y3 = nByte 3 e1
        y2 = nByte 2 e1
        y1 = nByte 1 e1
        
nByte n w = (.&.) 0xff $ shiftR w $ 8 * (n-1)

tfold :: (VType -> VType -> VType -> VType) -> VType -> VType
tfold lambdaYZ x = fold x 0 (lambdaYZ) x
          
-- Tests

testShl = TestCase (assertEqual "op1 Shl1"
                    [0, 2, 4, 6, 0x1fe, 0xFFFFFFFFfffffffe]
                    $ map (op1 Shl1)
                    [0, 1, 2, 3, 0x0ff, 0xFFFFFFFFffffffff])

testnByte = TestCase (assertEqual "op1 nByte"
                      ([0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11] :: [VType])
                      $ map (\n -> nByte n 0x1122334455667788)
                      [   1,    2,    3,    4,    5,    6,    7,    8])

tests = TestList [TestLabel "testShl" testShl
                 ,TestLabel "testnByte" testnByte
                 ]

testIt = runTestTT tests