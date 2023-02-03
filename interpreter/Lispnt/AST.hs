module Lispnt.AST where

type Identifier = String

data Pattern
    = PCall Pattern Pattern
    | PAtom Identifier
    | PVal Identifier
    deriving (Show)

data Behaviour
    = Define Pattern Expr
    deriving (Show)

data Expr
    = ECall Expr Expr
    | EAtom Identifier
    | EVal Identifier
    | EScope [Behaviour] Expr
    deriving (Show)