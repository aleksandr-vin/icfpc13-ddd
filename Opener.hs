

module Opener
( open
) where

import Data

open :: [Expr] -> [Expr]
open exs = concat $ map (openExpr) exs

openExpr :: Expr -> [Expr]
openExpr (Op1 op e) = map (op1Comb op) $ openExpr e
openExpr (Op2 op e1 e2) =
    let ops_e1 = map (op2Comb op) $ openExpr e1
    in concat $ map (\op_e1 -> map (op_e1) $ openExpr e2) ops_e1
openExpr (P Open) = [(P Zero), (P One), (P X)]


-----------------------
-- Оптимизации Op1

-- Оптимизация Not
op1Comb Not (P Zero) = (P One)
op1Comb Not (P One)  = (P Zero)
op1Comb Not e = (Op1 Not e)

-----------------------
-- Оптимизации Op2

-- Оптимизация And
op2Comb And (P Zero) _        = (P Zero)
op2Comb And (P One)  (P X)    = (Op2 And (P One) (P X))
op2Comb And (P One)  e        = e
op2Comb And (P X)    (P Zero) = (P Zero)
op2Comb And (P X)    (P One)  = op2Comb And (P One) (P X)
op2Comb And (P X)    (P X)    = (P X)
