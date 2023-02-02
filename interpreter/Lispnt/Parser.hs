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
    notProceeding $ parseString "def" <|> parseString "let"
    most1 (matchPred Ch.isLetter)
    where
        reversations = foldl (<|>) empty $ parseString <$> ["def", "let"]

parseType :: Parser Type
parseType = errlabel "expected type" $ Any <$ parseString "*" -- untyped

parsePolish :: (a -> a -> a) -> Parser a -> Parser a
parsePolish comb ma = ( do
    parseString "("
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

parseExprBase :: Parser Expr
parseExprBase = ExprValue <$> parseIdentifier

parsePatternBase :: Parser Pattern
parsePatternBase = (PatternValue <$> parseIdentifier)
    <|> (parseString "$" *> fmap PatternMatch parseIdentifier)

parseDef :: Parser TopLvlDef
parseDef = silence (parseString "def") *> context "def block" (do
    parseWhiteSpace
    name <- parseIdentifier
    (args, result) <- mostTill (do
        parseWhiteSpace
        parseType)
        (( do
        parseWhiteSpace
        parseString "="
        parseWhiteSpace
        parseType
        ) <|> pure Any)
    pure $ Def name args result)

parseLet :: Parser TopLvlDef
parseLet = silence (parseString "let") *> context "let block" (do
    parseWhiteSpace
    name <- parseIdentifier
    (args, result) <- mostTill (do
        parseWhiteSpace
        parsePolish PatternCall parsePatternBase)
        (do
        parseWhiteSpace
        parseString "="
        parseWhiteSpace
        parsePolishOpen ExprCall parseExprBase)
    pure $ Let name args result)

parseTopLevelDefinition :: Parser TopLvlDef
parseTopLevelDefinition = parseDef <|> parseLet <|> errmsg "expected top level definition"

parseTopLevel :: Parser [TopLvlDef]
parseTopLevel = (<$>) fst $ parseOptionalWhiteSpace *> most0PaddedTill parseTopLevelDefinition parseWhiteSpace (parseOptionalWhiteSpace *> eof)