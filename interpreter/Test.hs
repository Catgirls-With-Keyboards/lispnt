import Lispnt.Parser
import ParserComb (runParserT')
import Control.Monad.Identity (Identity(..))
import Control.Trans (EitherT(..))
import Data.Maybe (listToMaybe, fromMaybe)

errPrint :: String -> Err -> String
errPrint pre Silent = pre <> "a silent error occured"
errPrint pre (Err file i s) = pre <> s <> " at " <> show i <> " in file " <> file -- failed to xyz at ...
errPrint pre (Context file i s e) = errPrint pre e <> "\n" <> pre <> "in " <> s <> " at " <> show i <> " in file " <> file --  ... \n in secion abc at ...
errPrint pre (Bundle es) = concat (withBundleText . errPrint (pre <> "| ") <$> es) <> pre <> "...grouped here"
    where
        withBundleText txt = txt <> ('\n':pre) <> "|-- in bundle...\n"

main :: IO ()
main = do
    txt <- readFile "test.txt"

    putStrLn txt

    result <- runEitherT $ runParserT' "test.txt" (pure txt) $ parseOptionalWhiteSpace *> parseExpr <* parseOptionalWhiteSpace <* eof
    case result of
        (Left msg) -> putStrLn $ errPrint "" msg
        (Right res) -> print res
