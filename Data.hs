

module Data
( Expr(..)
, Expr'(..)
, Param(..)
, Param'(..)
) where


data Param = Open | Zero | One | X
     deriving (Show)

data Param' = Param Param | Y | Z
     deriving (Show)

data Expr = P Param
          | If0 Expr Expr Expr
          | Fold Expr Expr Expr'
          | TFold Expr'
          | Op1 Expr
          | Op2 Expr Expr
     deriving (Show)

data Expr' = P' Param'
           | If0' Expr' Expr' Expr'
           | Op1' Expr'
           | Op2' Expr' Expr'
     deriving (Show)
