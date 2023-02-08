module Lispnt.IR where

type Identifier = String

-- type Location = Int -- may use later for traceback

data Pattern
    = PCall Pattern Pattern
    | PAtom Identifier
    | PVal Identifier
    deriving (Show)

data Behaviour
    = Define Pattern Expr
    deriving (Show)

data Scope
    = Root
    | Child Scope [Behaviour]
    deriving (Show)

data Expr
    = ECall Expr Expr
    | EAtom Identifier Scope
    | EVal Identifier Scope
    deriving (Show)