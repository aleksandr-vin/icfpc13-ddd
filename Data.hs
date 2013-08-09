

module Data
( Expr(..)
, Expr'(..)
, Param(..)
, Param'(..)
, Op1(..)
, Op2(..)
, Operations(..)
) where


data Param = Open | Zero | One | X
     deriving (Eq)

data Param' = Param Param | Y | Z
     deriving (Eq)

data Expr = P Param
          | If0 Expr Expr Expr
          | Fold Expr Expr Expr'
          | TFold Expr'
          | Op1 Op1 Expr
          | Op2 Op2 Expr Expr
     deriving (Eq)

data Expr' = P' Param'
           | If0' Expr' Expr' Expr'
           | Op1' Op1 Expr'
           | Op2' Op2 Expr' Expr'
     deriving (Eq)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
     deriving (Eq)

data Op2 = And | Or | Xor | Plus
     deriving (Eq)

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

data Operations = OOp1 Op1
                | OOp2 Op2
                | OIf0
                | OFold
                | OTFold
     deriving (Eq, Show)
	 

instance Read Operations where
         readsPrec _ "not" = [(OOp1 Not, "")]
         readsPrec _ "shl1" = [(OOp1 Shl1, "")]
         readsPrec _ "shr4" = [(OOp1 Shr4, "")]
         readsPrec _ "shr16" = [(OOp1 Shr16, "")]
         readsPrec _ "shr1" = [(OOp1 Shr1, "")]
         readsPrec _ "and" = [(OOp2 And, "")]
         readsPrec _ "or" = [(OOp2 Or, "")]
         readsPrec _ "xor" = [(OOp2 Xor, "")]
         readsPrec _ "plus" = [((OOp2 Plus), "")]
         readsPrec _ "if0" = [(OIf0, "")]
         readsPrec _ "fold" = [(OFold, "")]
         readsPrec _ "tfold" = [(OTFold, "")]

