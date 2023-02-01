module AST where

type Identifier = String

data TopLvlDef
    = Def Identifier Int deriving (Show)