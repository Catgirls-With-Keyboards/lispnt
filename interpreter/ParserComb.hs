module ParserComb where
import Control.Applicative (Alternative(..))
import Control.Trans

data Ctx s = Ctx { index :: Int, rest :: [s] }
    deriving (Show)

data Err
    = Silent
    | Err Int String
    | Context Int String Err
    deriving (Show)

instance Semigroup Err where
    (<>) Silent = id
    (<>) message = const message

instance Monoid Err where
    mempty = Silent

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

most0InterlaceTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m (Either ([(a, b)], (a, c)) c)
most0InterlaceTill ma mb mc = (Left <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)) <|> (Right <$> mc)

least0InterlaceTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m (Either ([(a, b)], (a, c)) c)
least0InterlaceTill ma mb mc = (Right <$> mc) <|> (Left <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc))

most0Interlace :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m (Maybe ([(a, b)], a))
most0Interlace ma mb = do
    res <- most0InterlaceTill ma mb (pure ())
    case res of
        Right () -> pure Nothing
        Left (abs, (a, ())) -> pure $ Just (abs, a)

most1InterlaceTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([(a, b)], (a, c))
most1InterlaceTill ma mb mc = mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)

least1InterlaceTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([(a, b)], (a, c))
least1InterlaceTill ma mb mc = leastTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)

most1Interlace :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m ([(a, b)], a)
most1Interlace ma mb = fmap fst <$> most1InterlaceTill ma mb (pure ())

most0PaddedTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([a], c)
most0PaddedTill ma mb mc = do
    res <- most0InterlaceTill ma mb mc
    case res of
        Right c -> pure ([], c)
        Left (abs, (a, c)) -> pure ((fst <$> abs) ++ [a], c)

least0PaddedTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([a], c)
least0PaddedTill ma mb mc = do
    res <- least0InterlaceTill ma mb mc
    case res of
        Right c -> pure ([], c)
        Left (abs, (a, c)) -> pure ((fst <$> abs) ++ [a], c)

most0Padded :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m [a]
most0Padded ma mb = do
    res <- most0Interlace ma mb
    case res of
        Just (abs, a) -> pure $ (fst <$> abs) ++ [a]
        Nothing -> pure []

most1PaddedTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([a], c)
most1PaddedTill ma mb mc = do
    (abs, (a, c)) <- most1InterlaceTill ma mb mc
    pure ((fst <$> abs) ++ [a], c)

least1PaddedTill :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m c -> ParserT s m ([a], c)
least1PaddedTill ma mb mc = do
    (abs, (a, c)) <- least1InterlaceTill ma mb mc
    pure ((fst <$> abs) ++ [a], c)

most1Padded :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m b -> ParserT s m [a]
most1Padded ma mb = do
    (abs, a) <- most1Interlace ma mb
    pure $ (fst <$> abs) ++ [a]

notProceeding :: (Monad m, Alternative m) => ParserT s m a -> ParserT s m ()
notProceeding ma = do
    state <- get
    x <- True <$ ma <|> pure False -- garunteed to succeed
    set state
    if x then empty else pure ()

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

errBind :: (Monad m) => (e -> m e) -> ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
errBind f ma = StateT $ EitherT . runEitherT' (fmap Left . f) (pure . Right) . runStateT ma

label :: (Monad m) => m e -> ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
label e = errBind $ const e

silence :: (Monad m, Monoid e) => ParserT s (EitherT e m) a -> ParserT s (EitherT e m) a
silence = label $ pure mempty
