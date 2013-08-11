

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

--------------------------------------------------
--------------------------------------------------
-- Оптимизации Expr

--------------------------------------------------
-- Оптимизации Op1

-- Оптимизация Op1 Not
op1Comb Not (P Zero) = (P One)
op1Comb Not (P One)  = (P Zero)

-- остальные Op1 не оптимизируются
op1Comb op e = (Op1 op e)

--------------------------------------------------
-- Оптимизации Op2

-- Оптимизация Op2 And
op2Comb And (P Zero) _        = (P Zero)
op2Comb And (P One)  (P X)    = (Op2 And (P One) (P X))
op2Comb And (P One)  e        = e
op2Comb And (P X)    (P Zero) = (P Zero)
op2Comb And (P X)    (P One)  = op2Comb And (P One) (P X)
op2Comb And (P X)    (P X)    = (P X)

-- Оптимизация Op2 Or
op2Comb Or (P Zero) (P Zero)  = (P Zero)
op2Comb Or (P Zero) (P One)   = (P One)
op2Comb Or (P Zero) (P X)     = (P X)
op2Comb Or (P One)  (P Zero)  = (P One)
op2Comb Or (P One)  (P One)   = (P One)
op2Comb Or (P One)  (P X)     = (P X)
op2Comb Or (P X)    (P Zero)  = (P Zero)
op2Comb Or (P X)    (P One)   = (P X)
op2Comb Or (P X)    (P X)     = (P X)


-- Оптимизация Op2 Xor

-- Оптимизация Op2 Plus

op2Comb o e1 e2 = (Op2 o (min e1 e2) (max e1 e2))

--------------------------------------------------
-- Оптимизации TFold
tfoldComb (P' (Param Zero)) = (P Zero)
tfoldComb (P' (Param One))  = (P One)
tfoldComb (P' (Param X))    = (P X)
tfoldComb (P' Z)            = (P Zero)
tfoldComb e                 = (TFold e)



--------------------------------------------------
--------------------------------------------------
-- Оптимизации Expr'

--------------------------------------------------
-- Оптимизации Op1'

-- Оптимизация Op1' Not
op1Comb' Not (P' (Param Zero)) = (P' (Param One))
op1Comb' Not (P' (Param One))  = (P' (Param Zero))
 -- остальные Op1' не оптимизируются
op1Comb' op e = (Op1' op e)

--------------------------------------------------
-- Оптимизации Op2'

-- Оптимизация Op2' And
op2Comb' And (P' (Param Zero)) _                 = (P' (Param Zero))
op2Comb' And (P' (Param One))  (P' (Param X))    = (Op2' And (P' (Param One)) (P' (Param X)))
op2Comb' And (P' (Param One))  e                 = e
op2Comb' And (P' p)            (P' (Param Zero)) = (P' (Param Zero))
op2Comb' And (P' p)            (P' (Param One))  = op2Comb' And (P' (Param One)) (P' p)
op2Comb' And (P' p1)           (P' p2)
  | p1 == p2  = (P' p1)
  | otherwise = (Op2' And (P' (min p1 p2)) (P' (max p1 p2)))

-- Оптимизация Op2' Or
op2Comb' Or (P' (Param Zero)) (P' (Param Zero))   = (P' (Param Zero))
op2Comb' Or (P' (Param Zero)) (P' (Param One))    = (P' (Param One))
op2Comb' Or (P' (Param Zero)) e                   = e
op2Comb' Or (P' (Param One))  (P' (Param Zero))   = (P' (Param One))
op2Comb' Or (P' (Param One))  (P' (Param One))    = (P' (Param One))
op2Comb' Or (P' (Param One))  e                   = e
op2Comb' Or e                 (P' (Param Zero))   = (P' (Param Zero))
op2Comb' Or e                 (P' (Param One))    = (P' (Param X))
op2Comb' Or e1                e2                  
  | e1 == e2 = e1
  | otherwise = (Op2' Or (min e1 e2) (max e1 e2))

-- Оптимизация Op2' Xor

-- Оптимизация Op2' Plus

op2Comb' o e1 e2 = (Op2' o (min e1 e2) (max e1 e2))
