module Lispnt.AST where

type Identifier = String

data Type
    = Any
    deriving (Show)

data TopLvlDef
    = Def Identifier [Type] Type
    | Let Identifier [Pattern] Expr
    deriving (Show) 

data Pattern
    = PatternCall Pattern Pattern
    | PatternValue Identifier
    | PatternMatch Identifier
    deriving (Show)

data Expr
    = ExprCall Expr Expr
    | ExprValue Identifier
    deriving (Show)