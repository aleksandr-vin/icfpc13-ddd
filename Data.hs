

module Data
( Expr(..)
, Expr'(..)
, Param(..)
, Param'(..)
, Op1(..)
, Op2(..)
) where


data Param = Open | Zero | One | X
     deriving (Show)

data Param' = Param Param | Y | Z
     deriving (Show)

data Expr = P Param
          | If0 Expr Expr Expr
          | Fold Expr Expr Expr'
          | TFold Expr'
          | Op1 Op1 Expr
          | Op2 Op2 Expr Expr
     deriving (Show)

data Expr' = P' Param'
           | If0' Expr' Expr' Expr'
           | Op1' Op1 Expr'
           | Op2' Op2 Expr' Expr'
     deriving (Show)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
     deriving (Eq, Show)

data Op2 = And | Or | Xor | Plus
     deriving (Eq, Show)
