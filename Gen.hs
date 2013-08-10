

module Gen
( gen
, testIt
, fooGen
, fooSizer
) where

import Data
import Test.HUnit
import Data.List

-- import Debug.Trace
trace _ = id

gen :: Int -> [Operations] -> Expr
gen size ops =
          (Op1 Not (P Open))

-- Для отладки алгоритма перебора
fooGen :: (Char -> Int) -> Int -> String -> [String]
fooGen sizer n o
    | n == 0    = [""] -- [""]  при отладке меняем на ["+"]
    | otherwise = foldl yyy [] $ filter (\x -> 0 <= (decrN x)) o
    where yyy acc x =
              trace (";; acc = " ++ show acc ++ " x = " ++ show x) $
                        (addHead x) ++ acc
          addHead x =
              map (x:) $ trace (";;; xxx = " ++ show xxx)
                  xxx
              where xxx = fooGen (sizer) (decrN x) o
          decrN x = n - (sizer x)

-- Оценщик операции
sizer OFold = 2
sizer OTFold = 2
sizer _ = 1

fooSizer :: Char -> Int
fooSizer n
         | n == 'a'  = 1
         | otherwise = 1
               

-- Tests

test1 = TestCase (assertEqual "fooGenF"
      (Data.List.sort ["aaa"
                     ,"aab"
                     ,"aba"
                     ,"abb"
                     ,"baa"
                     ,"bab"
                     ,"bba"
                     ,"bbb"
                     ])
      (Data.List.sort (fooGen (fooSizer) 3 "ab")))

tests = TestList [ TestLabel "test1" test1
                 ]
testIt = runTestTT tests