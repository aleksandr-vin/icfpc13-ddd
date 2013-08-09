

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

data Operations = OOp1 Op1
                | OOp2 Op2
                | OIf0
                | OFold
                | OTFold
     deriving (Show, Eq)

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
