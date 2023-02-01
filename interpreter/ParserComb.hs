module ParserComb where
import Control.Applicative (Alternative(..))
import Control.Trans

data Ctx s = Ctx { index :: Int, rest :: [s] } deriving (Show)

newtype Err e = Err { msg :: e } deriving (Show)

instance Functor Err where
    fmap f = Err . f . msg

instance Semigroup (Err e) where
    (<>) = const

type ParserT s m a = StateT (Ctx s) m a

runParserT' :: (Monad m) => m [s] -> ParserT s m a -> m a
runParserT' = runStateT' . fmap (Ctx 0)

mostTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m ([a], b)
mostTill ma mb = ( do
        a <- ma
        (as, b) <- mostTill ma mb
        pure (a:as, b)
    ) <|> ( do
        b <- mb
        pure ([], b)
    )

leastTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m ([a], b)
leastTill ma mb = ( do
        b <- mb
        pure ([], b)
    ) <|> ( do
        a <- ma
        (as, b) <- mostTill ma mb
        pure (a:as, b)
    )

most :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m [a]
most ma = fst <$> mostTill ma (pure ())

most1Till :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m ([a], b)
most1Till ma mb = do
    a <- ma
    ( do
            (as, b) <- mostTill ma mb
            pure (a:as, b)
        ) <|> ( do
            b <- mb
            pure ([a], b)
        )

least1Till :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m ([a], b)
least1Till ma mb = do
    a <- ma
    ( do
            b <- mb
            pure ([a], b)
        ) <|> ( do
            (as, b) <- mostTill ma mb
            pure (a:as, b)
        )

most1 :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m [a]
most1 ma = fst <$> most1Till ma (pure ())

end :: (Monad m, Alternative m) => ParserT s m ()
end = do
    Ctx i ss <- get
    case ss of
        [] -> pure ()
        (s:ss') -> empty

matching :: (Monad m, Alternative m) => (s -> m a) -> ParserT s m a
matching f = do
    Ctx i ss <- get
    (s, ss') <- case ss of
        [] -> empty
        (s:ss') -> pure (s, ss')
    set $ Ctx (i + 1) ss'

    lift $ f s

matchPred :: (Monad m, Alternative m) => (s -> Bool) -> ParserT s m s
matchPred p = matching wrap
    where
        wrap s = if p s then pure s else empty

matchSection :: (Monad m, Alternative m, Eq s) => m [s] -> ParserT s m [s]
matchSection ms = lift ms >>= matchSection'
    where
        matchSection' :: (Monad m, Alternative m, Eq s) => [s] -> ParserT s m [s]
        matchSection' [] = pure []
        matchSection' (s:ss) = (:) <$> matchPred (== s) <*> matchSection' ss

errBind :: (Monad m, Alternative m) => (e -> m e) -> ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
errBind f ma = StateT $ EitherT . runEitherT' (fmap Left . f) (pure . Right) . runStateT ma

silence :: (Monad m, Alternative m, Monoid e) => ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
silence = errBind $ pure . const mempty

label :: (Monad m, Alternative m, Monoid e) => m e -> ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
label e = errBind $ (<*>) ((<>) <$> e) . pure
