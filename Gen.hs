

module Gen
( genP
, gen
, testIt
, fooGen
, fooSizer
, cons1
, cons2
) where

import Data
import Test.HUnit
import Data.List

--import Debug.Trace
trace _ = id

genP :: Int -> [Operations] -> [Prog]
genP n o = map (Prog) $ gen n o

gen :: Int -> [Operations] -> [Expr]
gen n o
    | n == 1    = [P Open]
    | otherwise = foldl yyy [] $ filter (\x -> 0 <= (decrN x)) o
    where yyy acc x = (addHead x) ++ acc
          addHead x =
              zzz x
              where zzz (OOp1 p) = cons1 (Op1 p) (decrN x) o
                    zzz (OOp2 p) = cons2 (Op2 p) (decrN x) o
                    zzz (OTFold) = consTFold (TFold) (decrN x) (filter (/= OTFold) o)
          decrN x = n - (sizer x)

gen' :: Int -> [Operations] -> [Expr']
gen' n o
    | n == 1    = [P' (Param Open)]
    | otherwise = trace (";; +") $ foldl yyy [] $ filter (\x -> 0 <= (decrN x)) o
    where yyy acc x = (addHead x) ++ acc
          addHead x =
              zzz x
              where zzz (OOp1 p) = cons1' (Op1' p) (decrN x) o
                    zzz (OOp2 p) = cons2' (Op2' p) (decrN x) o
          decrN x = n - (sizer x)

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
sizer OTFold = 2 + 1 + 1 -- Мы знаем что в tfold есть X и 0 -- это +2 к цене
sizer _ = 1

fooSizer :: Char -> Int
fooSizer n
         | n == 'a'  = 1
         | otherwise = 1

-- коннектор для унарных операций
--cons1 :: Op1 -> [Expr] -> [Expr]
consTFold op n o = map op $ gen' n o

cons1 op n o = map op $ gen n o

cons2 op n o = foldl (\acc (e1,e2) -> (mix e1 e2) ++ acc) [] plants
    where plants = [((gen i o), (gen (n-i) o)) | i <- [1..n `div` 2]] 
          mix e1 e2 = foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2

cons1' op n o = map op $ gen' n o

cons2' op n o = foldl (\acc (e1,e2) -> (mix e1 e2) ++ acc) [] plants
    where plants = [((gen' i o), (gen' (n-i) o)) | i <- [1..n `div` 2]] 
          mix e1 e2 = foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2

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