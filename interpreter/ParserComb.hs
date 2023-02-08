module ParserComb where
import Control.Applicative (Alternative(..))
import Control.Trans

data Ctx i s = Ctx { info :: i, index :: Integer, rest :: [s] }
    deriving (Show)

type ParserT i s m a = StateT (Ctx i s) m a

runParserT' :: (Monad m) => i -> m [s] -> ParserT i s m a -> m a
runParserT' ctxinfo = runStateT' . fmap (Ctx ctxinfo 0)

mostTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m ([a], b)
mostTill ma mb = ( do
        a <- ma
        (as, b) <- mostTill ma mb
        pure (a:as, b)
    ) <|> ( do
        b <- mb
        pure ([], b)
    )

leastTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m ([a], b)
leastTill ma mb = ( do
        b <- mb
        pure ([], b)
    ) <|> ( do
        a <- ma
        (as, b) <- mostTill ma mb
        pure (a:as, b)
    )

most :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m [a]
most ma = fst <$> mostTill ma (pure ())

most1Till :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m ([a], b)
most1Till ma mb = do
    a <- ma
    ( do
            (as, b) <- mostTill ma mb
            pure (a:as, b)
        ) <|> ( do
            b <- mb
            pure ([a], b)
        )

least1Till :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m ([a], b)
least1Till ma mb = do
    a <- ma
    ( do
            b <- mb
            pure ([a], b)
        ) <|> ( do
            (as, b) <- mostTill ma mb
            pure (a:as, b)
        )

most1 :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m [a]
most1 ma = fst <$> most1Till ma (pure ())

most0InterlaceTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m (Either ([(a, b)], (a, c)) c)
most0InterlaceTill ma mb mc = (Left <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)) <|> (Right <$> mc)

least0InterlaceTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m (Either ([(a, b)], (a, c)) c)
least0InterlaceTill ma mb mc = (Right <$> mc) <|> (Left <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc))

most0Interlace :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m (Maybe ([(a, b)], a))
most0Interlace ma mb = do
    res <- most0InterlaceTill ma mb (pure ())
    case res of
        Right () -> pure Nothing
        Left (abs, (a, ())) -> pure $ Just (abs, a)

most1InterlaceTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([(a, b)], (a, c))
most1InterlaceTill ma mb mc = mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)

least1InterlaceTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([(a, b)], (a, c))
least1InterlaceTill ma mb mc = leastTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)

most1Interlace :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m ([(a, b)], a)
most1Interlace ma mb = fmap fst <$> most1InterlaceTill ma mb (pure ())

most0PaddedTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([a], c)
most0PaddedTill ma mb mc = do
    res <- most0InterlaceTill ma mb mc
    case res of
        Right c -> pure ([], c)
        Left (abs, (a, c)) -> pure ((fst <$> abs) ++ [a], c)

least0PaddedTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([a], c)
least0PaddedTill ma mb mc = do
    res <- least0InterlaceTill ma mb mc
    case res of
        Right c -> pure ([], c)
        Left (abs, (a, c)) -> pure ((fst <$> abs) ++ [a], c)

most0Padded :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m [a]
most0Padded ma mb = do
    res <- most0Interlace ma mb
    case res of
        Just (abs, a) -> pure $ (fst <$> abs) ++ [a]
        Nothing -> pure []

most1PaddedTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([a], c)
most1PaddedTill ma mb mc = do
    (abs, (a, c)) <- most1InterlaceTill ma mb mc
    pure ((fst <$> abs) ++ [a], c)

least1PaddedTill :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m c -> ParserT i s m ([a], c)
least1PaddedTill ma mb mc = do
    (abs, (a, c)) <- least1InterlaceTill ma mb mc
    pure ((fst <$> abs) ++ [a], c)

most1Padded :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m b -> ParserT i s m [a]
most1Padded ma mb = do
    (abs, a) <- most1Interlace ma mb
    pure $ (fst <$> abs) ++ [a]

notProceeding :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m ()
notProceeding ma = do
    state <- get
    x <- True <$ ma <|> pure False -- garunteed to succeed
    set state
    if x then empty else pure ()

end :: (Monad m, Alternative m) => ParserT i s m ()
end = do
    Ctx ctxinfo i ss <- get
    case ss of
        [] -> pure ()
        (s:ss') -> empty

matching :: (Monad m, Alternative m) => (s -> m a) -> ParserT i s m a
matching f = do
    Ctx ctxinfo i ss <- get
    (s, ss') <- case ss of
        [] -> empty
        (s:ss') -> pure (s, ss')
    set $ Ctx ctxinfo (i + 1) ss'

    lift $ f s

matchPred :: (Monad m, Alternative m) => (s -> Bool) -> ParserT i s m s
matchPred p = matching wrap
    where
        wrap s = if p s then pure s else empty

matchSection :: (Monad m, Alternative m, Eq s) => m [s] -> ParserT i s m [s]
matchSection ms = lift ms >>= matchSection'
    where
        matchSection' :: (Monad m, Alternative m, Eq s) => [s] -> ParserT i s m [s]
        matchSection' [] = pure []
        matchSection' (s:ss) = (:) <$> matchPred (== s) <*> matchSection' ss

errBind :: (Monad m) => (e -> m f) -> ParserT i s (EitherT e m) a -> ParserT i s (EitherT f m) a
errBind f ma = StateT $ EitherT . runEitherT' (fmap Left . f) (pure . Right) . runStateT ma

label :: (Monad m) => m e -> ParserT i s (EitherT e m) a -> ParserT i s (EitherT e m) a
label e = errBind $ const e

silence :: (Monad m, Monoid e) => ParserT i s (EitherT e m) a -> ParserT i s (EitherT e m) a
silence = label $ pure mempty

skipWith :: (Monad m, Monoid e) => ParserT i s (EitherT e m) a -> ParserT i s (EitherT e m) b -> ParserT i s (EitherT e m) (EitherT e m a)
skipWith ma mb = do
    state <- get
    let eas = runStateT' (pure state) $ (,) <$> ma <*> get
    let ea = fst <$> eas
    ( do
        (a, state') <- silence $ lift eas
        set state'
        pure $ EitherT . pure . Right $ a
        ) <|> (ea <$ mb)

condense :: (Monad m, Monoid f) => ParserT i s (EitherT e m) [EitherT f m a] -> ParserT i s (EitherT e m) (EitherT f m [a])
condense ma = do
    foldr (\a as -> (:) <$> a <*> as) (pure []) <$> ma

bundle :: (Monad m) => ParserT i s (EitherT e m) (EitherT e m a) -> ParserT i s (EitherT e m) a
bundle ma = ma >>= lift
