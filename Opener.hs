

module Opener
( open
) where

import Data

open :: [Expr] -> [Expr]
open exs = concat $ map (openExpr) exs

openExpr :: Expr -> [Expr]
openExpr (Op1 op e) = map (Op1 op) $ openExpr e
openExpr (Op2 op e1 e2) =
    let ops_e1 = map (Op2 op) $ openExpr e1
    in concat $ map (\op_e1 -> map (op_e1) $ openExpr e2) ops_e1
openExpr (P Open) = [(P Zero), (P One), (P X)]

