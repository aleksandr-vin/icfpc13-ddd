

import Data
import Test.HUnit
import Data.List

generator :: Int -> [Operations] -> Expr
generator size ops =
          (Op1 Not (P Open))

-- Для отладки алгоритма перебора
fooGen :: Int -> String -> [String]
fooGen 0 _ = []
fooGen n [] = ["!"]
fooGen n o = (foldl (\acc x -> [x]:(addHead x) ++ acc) [] o)
       where addHead x = (map (x:) (fooGen (n-1) o))

fooGenF n o =
        filter ((n==) . length) $ fooGen n o

-- Tests

test1 = TestCase (assertEqual "generator 3 [not]"
      (Op1 Not (P Open))
      (generator 3 [read "not" :: Operations]))

test2 = TestCase (assertEqual "generator 5 [and]"
      (Op2 And (P Open) (P Open))
      (generator 5 [read "and" :: Operations]))

test3 = TestCase (assertEqual "fooGenF"
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
                 , TestLabel "test2" test2
                 , TestLabel "test3" test3
                 ]

testIt = runTestTT tests