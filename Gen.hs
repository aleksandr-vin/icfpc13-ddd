

module Gen
( gen
, testIt
) where

import Data
import Test.HUnit
import Data.List

gen :: Int -> [Operations] -> Expr
gen size ops =
          (Op1 Not (P Open))

-- Для отладки алгоритма перебора
fooGen :: (Char -> Int) -> Int -> String -> [String]
fooGen _ 0 _ = []
fooGen sizer n o = (foldl (\acc x -> [x]:(addHead x) ++ acc) [] o)
       where addHead x = (map (x:) (fooGen (sizer) (n - (sizer x)) o))

-- Оценщик операции
sizer OFold = 2
sizer OTFold = 2
sizer _ = 1

fooGenF n o =
        filter ((n==) . length) $ fooGen (fooSizer) n o

fooSizer :: Char -> Int
fooSizer n = 1

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
      (Data.List.sort (fooGenF 3 "ab")))

tests = TestList [ TestLabel "test1" test1
                 ]
testIt = runTestTT tests