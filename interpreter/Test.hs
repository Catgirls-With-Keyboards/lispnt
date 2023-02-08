import Lispnt.Parser
import ParserComb (runParserT', Err (..))
import Control.Monad.Identity (Identity(..))
import Control.Trans (EitherT(..))
import Data.Maybe (listToMaybe, fromMaybe)

-- at i = "at ..."
-- prettyPrint :: (Integer -> String) -> Err -> String
-- prettyPrint at Silent = "a silent error occured"
-- prettyPrint at (Err file i s) = s <> " " <> at i <> "in file " <> file -- failed to xyz at ...
-- prettyPrint at (Context file i s e) = prettyPrint at e <> "\nin " <> s <> " " <> at i <> "in file " <> file --  ... \n in secion abc at ...

errPrint :: Err -> String
errPrint Silent = "a silent error occured"
errPrint (Err file i s) = s <> " at " <> show i <> " in file " <> file -- failed to xyz at ...
errPrint (Context file i s e) = errPrint e <> "\nin " <> s <> " at " <> show i <> " in file " <> file --  ... \n in secion abc at ...

main :: IO ()
main = do
    txt <- readFile "test.txt"

    putStrLn txt

    -- let nlOccurances = reverse $ [n | (c, n) <- txt `zip` [0..], c == '\n'] `zip` [1..]
    -- let lineChar i = fromMaybe (0, i) $ listToMaybe [(l, i - (n + 1)) | (n, l) <- nlOccurances, n < i]
    -- let formatLn (l, n) = "at " <> show l <> ":" <> show n
    -- let errPrint = prettyPrint (formatLn . lineChar)

    result <- runEitherT $ runParserT' "test.txt" (pure txt) $ parseOptionalWhiteSpace *> parseExpr <* parseOptionalWhiteSpace <* eof
    case result of
        (Left msg) -> putStrLn $ errPrint msg
        (Right res) -> print res
