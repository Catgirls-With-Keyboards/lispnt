module Lispnt.Parser where
import Control.Applicative (Alternative(..), optional)
import Control.Trans
import Control.Monad.Identity (Identity)
import ParserComb
import Data.Char as Ch
import Lispnt.AST
import Data.Functor.Identity (Identity(..))
import System.Posix

data Err
    = Silent
    | Err String Integer String
    | Context String Integer String Err
    deriving (Show)

instance Semigroup Err where
    (<>) Silent = id
    (<>) message = const message

instance Monoid Err where
    mempty = Silent

type Parser a = ParserT String Char (EitherT Err IO) a

-- msg = "failed to xyz"
errlabel :: String -> Parser a -> Parser a
errlabel msg ma = do
    Ctx file i _ <- get
    label (pure $ Err file i msg) ma

-- msg = expected u, v or w
errmsg :: String -> Parser a
errmsg msg = do
    Ctx file i _ <- get
    lift (EitherT . pure . Left $ Err file i msg)

-- msg = "section abc"
context :: String -> Parser a -> Parser a
context msg ma = do
    Ctx file i _ <- get
    errBind (pure . Context file i msg) ma

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

parseStringChar :: Parser Char
parseStringChar = silence (parseString "\\") *> errlabel "expected valid escaped charecter" (
        (<$) '\\' (matchPred (== '\\'))
        <|> (<$) '\'' (matchPred (== '\"'))
        <|> (<$) '\n' (matchPred (== 'n'))
        <|> (<$) '\t' (matchPred (== 't'))
    ) <|> silence (notProceeding (matchPred (== '\"')) *> matchPred (const True))

parseStringLit :: Parser String
parseStringLit = fmap fst $ silence (parseString "\"") *> mostTill parseStringChar terminator
    where
        terminator = silence (parseString "\"") <|> errmsg "expected char or end of string"

parsePolish :: (a -> a -> a) -> Parser a -> Parser a
parsePolish comb ma = ( do
    silence $ parseString "("
    func <- ( do
        parseOptionalWhiteSpace
        self
        )
    args <- most ( do
        parseOptionalWhiteSpace -- parseWhiteSpace -- making this optional may turn out to be a terrible idea
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
        parseOptionalWhiteSpace -- parseWhiteSpace -- making this optional may turn out to be a terrible idea
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
            <|> ( do
                silence $ parseString "{"
                parseOptionalWhiteSpace
                (behaviours, _) <- most0PaddedTill
                    parseBehaviour
                    parseOptionalWhiteSpace
                    $ parseOptionalWhiteSpace *> parseString "}"
                parseOptionalWhiteSpace
                EScope behaviours <$> parseExpr
            )
            <|> ( do
                folder <- parseStringLit
                parseOptionalWhiteSpace
                parseString "/"
                parseOptionalWhiteSpace
                file <- parseStringLit <|> errmsg "expected \""
                parseFile folder file
            )
            <|> errmsg "expected identifier, scope or import"

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

parseFile :: String -> String -> Parser Expr
parseFile folder file = do
    here <- lift $ lift getWorkingDirectory
    lift $ lift $ changeWorkingDirectory folder
    source <- lift $ lift $ readFile file
    state <- get
    set $ Ctx (folder ++ "/" ++ file) 0 source
    let final = do
        set state
        lift $ lift $ changeWorkingDirectory here
    expr <- parseExpr <|> (final *> empty)
    final
    pure expr