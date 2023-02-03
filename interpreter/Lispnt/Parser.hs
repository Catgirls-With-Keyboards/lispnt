module Lispnt.Parser where
import Control.Applicative (Alternative(..), optional)
import Control.Trans
import Control.Monad.Identity (Identity)
import ParserComb
import Data.Char as Ch
import Lispnt.AST
import Data.Functor.Identity (Identity(..))

type Parser a = ParserT Char (EitherT Err Identity) a

-- msg = "failed to xyz"
errlabel :: String -> Parser a -> Parser a
errlabel msg ma = do
    i <- index <$> get
    label (pure $ Err i msg) ma

-- msg = expected u or v or w
errmsg :: String -> Parser a
errmsg msg = do
    i <- index <$> get
    lift (EitherT . Identity . Left $ Err i msg)


-- msg = "section abc"
context :: String -> Parser a -> Parser a
context msg ma = do
    i <- index <$> get
    errBind (pure . Context i msg) ma

eof :: Parser ()
eof = errlabel "expected end of file" end

parseString :: String -> Parser String
parseString str = errlabel ("expected " <> show str) $ matchSection $ pure str

parseOptionalWhiteSpace :: Parser String
parseOptionalWhiteSpace = most (matchPred Ch.isSpace)

parseWhiteSpace :: Parser String
parseWhiteSpace = errlabel "expected spacing" $ most1 (matchPred Ch.isSpace)

parseIdentifier :: Parser Identifier
parseIdentifier = errlabel "expected identifier" $ do
    most1 (matchPred Ch.isLetter)

parsePolish :: (a -> a -> a) -> Parser a -> Parser a
parsePolish comb ma = ( do
    silence $ parseString "("
    func <- ( do
        parseOptionalWhiteSpace
        self
        )
    args <- most ( do
        parseWhiteSpace
        self
        )
    parseOptionalWhiteSpace
    parseString ")"
    pure $ foldl comb func args
    ) <|> ma
    where
        self = parsePolish comb ma

parsePolishOpen :: (a -> a -> a) -> Parser a -> Parser a
parsePolishOpen comb ma = do
    func <- ( do
        base
        )
    args <- most ( do
        parseWhiteSpace
        base
        )
    pure $ foldl comb func args
    where
        base = parsePolish comb ma

parsePattern :: Parser Pattern
parsePattern = context "pattern" $ parsePolishOpen PCall base
    where
        base = fmap PAtom (silence $ parseString ":" *> parseIdentifier)
            <|> fmap PVal (silence parseIdentifier)
            <|> errmsg "expected identifier"

parseExpr :: Parser Expr
parseExpr = context "expression" $ parsePolishOpen ECall base
    where
        base = fmap EAtom (silence $ parseString ":" *> parseIdentifier)
            <|> fmap EVal (silence parseIdentifier)
            <|> (do
                silence $ parseString "{"
                parseOptionalWhiteSpace
                (behaviours, _) <- most0PaddedTill
                    parseBehaviour
                    parseOptionalWhiteSpace
                    $ parseOptionalWhiteSpace *> parseString "}"
                parseOptionalWhiteSpace
                EScope behaviours <$> parseExpr
            )
            <|> errmsg "expected identifier or scope"

parseBehaviour :: Parser Behaviour
parseBehaviour = context "behaviour definition" $ do
    pat <- parsePattern
    parseOptionalWhiteSpace
    parseString "="
    parseOptionalWhiteSpace
    exp <- parseExpr
    parseOptionalWhiteSpace
    parseString ";"
    pure $ Define pat exp
