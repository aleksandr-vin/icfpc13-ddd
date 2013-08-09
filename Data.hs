

module Data
( Expr(..)
, Expr'(..)
, Param(..)
, Param'(..)
, Op1(..)
, Op2(..)
) where


data Param = Open | Zero | One | X

data Param' = Param Param | Y | Z

data Expr = P Param
          | If0 Expr Expr Expr
          | Fold Expr Expr Expr'
          | TFold Expr'
          | Op1 Op1 Expr
          | Op2 Op2 Expr Expr

data Expr' = P' Param'
           | If0' Expr' Expr' Expr'
           | Op1' Op1 Expr'
           | Op2' Op2 Expr' Expr'

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16

data Op2 = And | Or | Xor | Plus


instance Show Param where
  show Open = "?"
  show Zero = "0"
  show One = "1"
  show X = "X"

instance Show Param' where
  show (Param a) = show a
  show Y = "Y"  
  show Z = "Z"  
  
instance Show Op1 where
  show Not = "not"
  show Shl1 = "shl1"
  show Shr1 = "shr1"
  show Shr4 = "shr4"
  show Shr16 = "shr16"

instance Show Op2 where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Plus = "plus"

instance Show Expr' where
  show (P' a) = show a
  show (If0' a b c) = "(if0 "++show a++" "++show b++" "++show c++")"
  show (Op1' a b) = show a++" "++show b
  show (Op2' a b c) = show a++" "++show b++" "++show c
    
  
instance Show Expr where
  show (P a) = show a
  show (If0 a b c) = "(if0 "++show a++" "++show b++" "++show c++")"
  show (Fold a b c) = "(fold "++show a++" "++show b++" (lambda(y z) ("++show c++"))"
  show (TFold a) = "(fold "++show X++" "++show Zero++" (lambda("++show X++"y) ("++show a++"))"
  show (Op1 a b) = show a++" "++show b
  show (Op2 a b c) = show a++" "++show b++" "++show c