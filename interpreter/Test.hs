import Lispnt.Parser
import ParserComb (runParserT', Err (..))
import Control.Monad.Identity (Identity(..))
import Control.Trans (EitherT(..))
import Data.Maybe (listToMaybe, fromMaybe)

-- at i = "at ..."
prettyPrint :: (Int -> String) -> Err -> String
prettyPrint at Silent = "a silent error occured"
prettyPrint at (Err i s) = s <> " " <> at i -- failed to xyz at ...
prettyPrint at (Context i s e) = prettyPrint at e <> "\nin " <> s <> " " <> at i --  ... \n in secion abc at ...

main :: IO ()
main = do
    txt <- readFile "test.txt"

    putStrLn txt

    let nlOccurances = reverse $ [n | (c, n) <- txt `zip` [0..], c == '\n'] `zip` [1..]
    let lineChar i = fromMaybe (0, i) $ listToMaybe [(l, i - (n + 1)) | (n, l) <- nlOccurances, n < i]
    let formatLn (l, n) = "at " <> show l <> ":" <> show n
    let errPrint = putStrLn . prettyPrint (formatLn . lineChar)

    let result = runParserT' (pure txt) $ parseOptionalWhiteSpace *> parseExpr <* parseOptionalWhiteSpace <* eof
    case result of
        EitherT (Identity (Left msg)) -> errPrint msg
        EitherT (Identity (Right res)) -> print res
