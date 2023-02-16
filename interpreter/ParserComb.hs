module ParserComb where
import Control.Applicative (Alternative(..))
import Control.Trans

data Ctx i s = Ctx { info :: i, index :: Integer, rest :: [s] }
    deriving (Show)

type ParserT i s m a = StateT (Ctx i s) m a

runParserT' :: (Monad m) => i -> m [s] -> ParserT i s m a -> m a
runParserT' ctxinfo = runStateT' . fmap (Ctx ctxinfo 0)

notProceeding :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m ()
notProceeding ma = do
    state <- get
    x <- True <$ ma <|> pure False -- garunteed to succeed
    set state
    if x then empty else pure ()

proceeding :: (Monad m, Alternative m) => ParserT i s m a -> ParserT i s m a
proceeding ma = do
    state <- get
    a <- ma
    set state
    pure a

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

matchAll :: (Monad m, Alternative m) => ParserT i s m s
matchAll = matchPred $ const True

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

