

module Opener
( open
, openP
) where

import Data
import Data.List

open = nub . openExprs

-- Открывает параметры в Prog
openP :: [Prog] -> [Prog]
openP progs =
    map (Prog) . open $ map (unProg) progs
        where unProg (Prog e) = e

openExprs :: [Expr] -> [Expr]
openExprs exs = concat $ map (openExpr) exs

openExpr :: Expr -> [Expr]
openExpr (Op1 op e) = map (op1Comb op) $ openExpr e
openExpr (Op2 op e1 e2) =
    let ops_e1 = map (op2Comb op) $ openExpr e1
    in concat $ map (\op_e1 -> map (op_e1) $ openExpr e2) ops_e1
openExpr (TFold e) = map (tfoldComb) $ openExpr' e
openExpr (P Open) = [(P Zero), (P One), (P X)]
openExpr e = [e]

openExpr' :: Expr' -> [Expr']
openExpr' (Op1' op e) = map (op1Comb' op) $ openExpr' e
openExpr' (Op2' op e1 e2) =
    let ops_e1 = map (op2Comb' op) $ openExpr' e1
    in concat $ map (\op_e1 -> map (op_e1) $ openExpr' e2) ops_e1
openExpr' (P' (Param Open)) = [(P' (Param Zero))
                              ,(P' (Param One))
                              ,(P' (Param X))
                              ,(P' Y)
                              ,(P' Z)]
openExpr' e = [e]                              

-- Заменяет шаблонный eT на eN в e
subsExp' :: Expr' -> Expr' -> Expr' -> Expr'
subsExp' eT eN (Op1' op e) = op1Comb' op $ subsExp' eT eN e
subsExp' eT eN (Op2' op e1 e2) = op2Comb' op (subsExp' eT eN e1) $ subsExp' eT eN e2
subsExp' eT eN e
  | e == eT = eN
  | otherwise = e

--------------------------------------------------
--------------------------------------------------
-- Оптимизации Expr

--------------------------------------------------
-- Оптимизации Op1

-- Оптимизация Op1 Not
op1Comb Not (Op1 Not e) = e
-- остальные Op1 не оптимизируются
op1Comb op e = (Op1 op e)

--------------------------------------------------
-- Оптимизации Op2

-- Оптимизация Op2 And
op2Comb And (P Zero) _        = (P Zero)
op2Comb And (P One)  (P Zero) = (P Zero)
op2Comb And (P One)  (P One)  = (P One)
op2Comb And (P One)  e        = (Op2 And (min (P One) e) (max (P One) e))
op2Comb And _        (P Zero) = (P Zero)
op2Comb And e        (P One)  = op2Comb And (P One) e
op2Comb And e1       e2
  | e1 == e2 = e1
  | otherwise = (Op2 And (min e1 e2) (max e1 e2))

-- Оптимизация Op2 Or
op2Comb Or (P Zero) (P Zero)  = (P Zero)
op2Comb Or (P Zero) (P One)   = (P One)
op2Comb Or (P Zero) e         = e
op2Comb Or (P One)  (P Zero)  = (P One)
op2Comb Or (P One)  (P One)   = (P One)
op2Comb Or (P One)  e         = (Op2 Or (min (P One) e) (max (P One) e))
op2Comb Or e        (P Zero)  = e
op2Comb Or e        (P One)   = op2Comb Or (P One) e
op2Comb Or e1       e2            
  | e1 == e2 = e1
  | otherwise = (Op2 Or (min e1 e2) (max e1 e2))

-- Оптимизация Op2 Xor

-- Оптимизация Op2 Plus
op2Comb Plus (P Zero) (P Zero) = (P Zero)
op2Comb Plus (P Zero) (P One)  = (P One)
op2Comb Plus (P Zero) e        = e
op2Comb Plus (P One)  (P Zero) = (P One)
op2Comb Plus (P One)  (P One)  = (Op1 Shl1 (P One)) -- Уменьшаем цену 3->2
op2Comb Plus (P One)  e        = (Op2 Plus
                                  (min (P One) e)
                                  (max (P One) e))
op2Comb Plus e        (P Zero) = e
op2Comb Plus e        (P One)  = op2Comb Plus (P One) e
op2Comb Plus e1       e2
  | e1 == e2 = op1Comb Shl1 e1 -- М.б. там будут ещё опт. в завис. от e1
  | otherwise = (Op2 Plus (min e1 e2) (max e1 e2))

-- остальные Op2 не оптимизируются
op2Comb o e1 e2 = (Op2 o (min e1 e2) (max e1 e2))

--------------------------------------------------
-- Оптимизации TFold
tfoldComb (P' (Param Zero)) = (P Zero)
tfoldComb (P' (Param One))  = (P One)
tfoldComb (P' (Param X))    = (P X)
tfoldComb (P' Z)            = (P Zero)
-- Оптимизации TFold second-level
tfoldComb e = let assumption = subsExp' (P' Z) (P' (Param Zero)) e
              in if assumption == (P' (Param Zero))
              then (P Zero)
              else (TFold e)



--------------------------------------------------
--------------------------------------------------
-- Оптимизации Expr'

--------------------------------------------------
-- Оптимизации Op1'

-- Оптимизация Op1' Not
op1Comb' Not (Op1' Not e) = e
-- остальные Op1' не оптимизируются
op1Comb' op e = (Op1' op e)

--------------------------------------------------
-- Оптимизации Op2'

-- Оптимизация Op2' And
op2Comb' And (P' (Param Zero)) _                 = (P' (Param Zero))
op2Comb' And (P' (Param One))  (P' (Param Zero)) = (P' (Param Zero))
op2Comb' And (P' (Param One))  (P' (Param One))  = (P' (Param One))
op2Comb' And (P' (Param One))  e                 = (Op2' And (min (P' (Param One)) e)
                                                    (max (P' (Param One)) e))
op2Comb' And _                 (P' (Param Zero)) = (P' (Param Zero))
op2Comb' And e                 (P' (Param One))  = op2Comb' And (P' (Param One)) e
op2Comb' And e1                e2
  | e1 == e2  = e1
  | otherwise = (Op2' And (min e1 e2) (max e1 e2))

-- Оптимизация Op2' Or
op2Comb' Or (P' (Param Zero)) (P' (Param Zero))   = (P' (Param Zero))
op2Comb' Or (P' (Param Zero)) (P' (Param One))    = (P' (Param One))
op2Comb' Or (P' (Param Zero)) e                   = e
op2Comb' Or (P' (Param One))  (P' (Param Zero))   = (P' (Param One))
op2Comb' Or (P' (Param One))  (P' (Param One))    = (P' (Param One))
op2Comb' Or (P' (Param One))  e                   = (Op2' Or (min (P' (Param One)) e)
                                                     (max (P' (Param One)) e))
op2Comb' Or e                 (P' (Param Zero))   = e
op2Comb' Or e                 (P' (Param One))    = op2Comb' Or (P' (Param One)) e
op2Comb' Or e1                e2                  
  | e1 == e2 = e1
  | otherwise = (Op2' Or (min e1 e2) (max e1 e2))

-- Оптимизация Op2' Xor

-- Оптимизация Op2' Plus
op2Comb' Plus (P' (Param Zero)) (P' (Param Zero)) = (P' (Param Zero))
op2Comb' Plus (P' (Param Zero)) (P' (Param One))  = (P' (Param One))
op2Comb' Plus (P' (Param Zero)) e                 = e
op2Comb' Plus (P' (Param One))  (P' (Param Zero)) = (P' (Param One))
op2Comb' Plus (P' (Param One))  (P' (Param One))  = (Op1' Shl1 (P' (Param One))) -- Уменьшаем цену 3->2
op2Comb' Plus (P' (Param One))  e                 = (Op2' Plus
                                                     (min (P' (Param One)) e)
                                                     (max (P' (Param One)) e))
op2Comb' Plus e                 (P' (Param Zero)) = e
op2Comb' Plus e                 (P' (Param One))  = op2Comb' Plus (P' (Param One)) e
op2Comb' Plus e1                e2
  | e1 == e2 = op1Comb' Shl1 e1 -- М.б. там будут ещё опт. в завис. от e1
  | otherwise = (Op2' Plus (min e1 e2) (max e1 e2))

-- остальные Op2' не оптимизируются
op2Comb' o e1 e2 = (Op2' o (min e1 e2) (max e1 e2))
