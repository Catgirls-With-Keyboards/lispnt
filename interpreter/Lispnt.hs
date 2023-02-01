module Lispnt where
import Control.Applicative (Alternative(..), optional)
import Control.Trans
import Control.Monad.Identity (Identity)
import ParserComb
import Data.Char as Ch
import AST

type Error = Maybe (Err String)

raise :: String -> Error
raise = pure . Err

type Parser a = ParserT Char (EitherT Error Identity) a

parseWhiteSpace :: Parser String
parseWhiteSpace = most (matchPred Ch.isSpace)

parseIdentifier :: Parser Identifier
parseIdentifier = most1 (matchPred Ch.isLetter)

parseDef :: Parser TopLvlDef
parseDef = do
    matchSection $ pure "def"
    parseWhiteSpace
    name <- parseIdentifier
    parseWhiteSpace
    args <- most $ (1 :: Int) <$ do
        matchSection $ pure "*"
        parseWhiteSpace
    optional $ () <$ do
        matchSection $ pure "="
        parseWhiteSpace
        matchSection $ pure "*"
        parseWhiteSpace
    pure $ Def name (sum args)


