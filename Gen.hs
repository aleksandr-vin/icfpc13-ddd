

module Gen
( genP
, gen
, genE
, testIt
) where

import Data
import Test.HUnit
import Data.List

--import Debug.Trace
trace _ = id

---------------
permuts n = nub . concat $ [[[k, i, (n-i-k)] | i <- [1..n-k-1]] | k <- [1..n-2]]
---------------

filterExps o exs = filter (\p -> opsInProg (remapTFold o) p) exs
    where opsInProg o p = all (\x -> isInfixOf x (show p)) o
          remapTFold o = map (\x -> if x == "tfold" then "fold" else x) o

gen n o = filterExps (map (show) o) $ genE n o

genP :: Int -> [Operations] -> [Prog]
genP n o = map (Prog) $ gen n o

genE :: Int -> [Operations] -> [Expr]
genE n o
    | n == 1    = [P Open]
    | otherwise = foldl yyy [] $ filter (\x -> 0 <= (decrN x)) o
    where yyy acc x = (addHead x) ++ acc
          addHead x =
              zzz x
              where zzz (OOp1 p) = cons1 (Op1 p) (decrN x) o
                    zzz (OOp2 p) = cons2 (Op2 p) (decrN x) o
                    zzz (OTFold) = consTFold (TFold) (decrN x)
                                   (filter (\x -> x /= OTFold && x /= OFold) o)
                    zzz (OIf0)   = consIf0 (If0) (decrN x) o
                    zzz (OFold)  = consFold (Fold) (decrN x) o
          decrN x = n - (sizer x)

genE' :: Int -> [Operations] -> [Expr']
genE' n o
    | n == 1    = [P' (Param Open)]
    | otherwise = trace (";; +") $ foldl yyy [] $ filter (\x -> 0 <= (decrN x)) o
    where yyy acc x = (addHead x) ++ acc
          addHead x =
              zzz x
              where zzz (OOp1 p) = cons1' (Op1' p) (decrN x) o
                    zzz (OOp2 p) = cons2' (Op2' p) (decrN x) o
                    zzz (OIf0)   = consIf0' (If0') (decrN x) o
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
consTFold op n o = map op $ genE' n o

cons1 op n o = map op $ genE n o

-- коннектор для бинарных операций
cons2 op n o = foldl (\acc (e1,e2) -> (mix e1 e2) ++ acc) [] plants
    where plants = [((genE i o), (genE (n-i) o)) | i <- [1..n `div` 2]] 
          mix e1 e2 = foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2

-- коннектор для тернарных операций
consIf0 op n o =
  trace (";;== plants = "++ show plants) $
  foldl (\acc (e1,e2,e3) -> (mix e1 e2 e3) ++ acc) [] plants
  where plants = [(genE a o, genE b o, genE c o) | [a,b,c] <- permuts n] 
        mix e1 e2 e = mix' (foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2) e
        mix' op' e3 = foldl (\acc e3x -> (map (\opx -> opx e3x) op') ++ acc) [] e3
        
consFold op n o =
  trace (";;== plants = "++ show plants) $
  foldl (\acc (e1,e2,e3) -> (mix e1 e2 e3) ++ acc) [] plants
  where plants = [(genE a o, genE b o, genE' c o) | [a,b,c] <- permuts n] 
        mix e1 e2 e = mix' (foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2) e
        mix' op' e3 = foldl (\acc e3x -> (map (\opx -> opx e3x) op') ++ acc) [] e3

cons1' op n o = map op $ genE' n o

cons2' op n o = foldl (\acc (e1,e2) -> (mix e1 e2) ++ acc) [] plants
    where plants = [((genE' i o), (genE' (n-i) o)) | i <- [1..n `div` 2]] 
          mix e1 e2 = foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2

consIf0' op n o = -- [op (P Open) (P Open) (P Open)]
  trace (";;== plants = "++ show plants) $
  foldl (\acc (e1,e2,e3) -> (mix e1 e2 e3) ++ acc) [] plants
  where plants = [(genE' a o, genE' b o, genE' c o) | [a,b,c] <- permuts n] 
        mix e1 e2 e = mix' (foldl (\acc e2x -> (map (\e1x -> op e1x e2x) e1) ++ acc) [] e2) e
        mix' op' e3 = foldl (\acc e3x -> (map (\opx -> opx e3x) op') ++ acc) [] e3

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
        
testPermuts = TestCase (assertEqual "permuts"
                        [[1,1,4],[1,2,3],[1,3,2],[1,4,1],[2,1,3],[2,2,2],[2,3,1],[3,1,2],[3,2,1],[4,1,1]]
                        $ permuts 6)
      
tests = TestList [TestLabel "test1" test1
                 ,TestLabel "testPermuts" testPermuts
                 ]

testIt = runTestTT tests