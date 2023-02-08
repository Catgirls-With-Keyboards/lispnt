{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
module Control.Trans where
import Control.Applicative (Alternative(..))

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: (Monad m) => m a -> t m a
    
{- MaybeT -}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

runMaybeT' :: (Monad m) => m b -> (a -> m b) -> MaybeT m a -> m b
runMaybeT' mb f mma = do
    ma <- runMaybeT mma
    maybe mb f ma

instance (forall a'. Show a' => Show (m a'), Show a) => Show (MaybeT m a) where
    show = show . runMaybeT

instance (Functor f) => Functor (MaybeT f) where
    fmap f = MaybeT . (fmap . fmap) f . runMaybeT

instance (Applicative f) => Applicative (MaybeT f) where
    pure = MaybeT . pure . pure
    (<*>) = (MaybeT .) . (. runMaybeT) . (<*>) . (<$>) (<*>) . runMaybeT

instance (Applicative f) => Alternative (MaybeT f) where
    empty = MaybeT $ pure Nothing
    (<|>) (MaybeT ma) (MaybeT mb) = MaybeT $ (<|>) <$> ma <*> mb

instance (Monad m) => Monad (MaybeT m) where
    (>>=) = flip $ (MaybeT .) . (. runMaybeT) . travbind . (runMaybeT .)
        where
            travbind :: (Monad m) => (a -> m (Maybe b)) -> (m (Maybe a) -> m (Maybe b))
            travbind f = (=<<) $ maybe (pure Nothing) f

instance MonadTrans MaybeT where
    lift = MaybeT . (<$>) pure

{- EitherT -}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

runEitherT' :: (Monad m) => (e -> m b) -> (a -> m b) -> EitherT e m a -> m b
runEitherT' f g mma = do
    ma <- runEitherT mma
    either f g ma

instance (forall a'. Show a' => Show (m a'), Show a, Show e) => Show (EitherT e m a) where
    show = show . runEitherT

instance (Functor f) => Functor (EitherT e f) where
    fmap f = EitherT . (fmap . fmap) f . runEitherT

instance (Applicative f) => Applicative (EitherT e f) where
    pure = EitherT . pure . pure
    (<*>) = (EitherT .) . (. runEitherT) . (<*>) . (<$>) (<*>) . runEitherT

instance (Applicative f, Monoid e) => Alternative (EitherT e f) where
    empty = EitherT . pure $ Left mempty
    (<|>) (EitherT ma) (EitherT mb) = EitherT $ choose <$> ma <*> mb
        where
            choose ma@(Right _) _ = ma
            choose _ mb@(Right _) = mb
            choose (Left a) (Left b) = Left $ a <> b

instance (Monad m) => Monad (EitherT e m) where
    (>>=) = flip $ (EitherT .) . (. runEitherT) . travbind . (runEitherT .)
        where
            travbind :: (Monad m) => (a -> m (Either e b)) -> (m (Either e a) -> m (Either e b))
            travbind f = (=<<) $ either (pure . Left) f

instance MonadTrans (EitherT e) where
    lift = EitherT . (<$>) pure

{- StateT -}

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- retrieves the 'background' value
get :: (Applicative m) => StateT s m s
get = StateT $ \s -> pure (s, s)

-- assigns to the 'background' value
set :: (Applicative m) => s -> StateT s m ()
set s = StateT $ \s' -> pure (s, ())

runStateT' :: (Monad m) => m s -> StateT s m a -> m a
runStateT' ms mma = do
    s <- ms
    snd <$> runStateT mma s

instance (Functor f) => Functor (StateT s f) where
    fmap f = StateT . (fmap . fmap . fmap) f . runStateT

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ pure . (, a)
    (StateT mf) <*> (StateT ma) = StateT $ \s -> do
        (s', f) <- mf s
        (s'', a) <- ma s'
        pure (s'', f a)

instance (Alternative m, Monad m) => Alternative (StateT s m) where
    empty = StateT $ const empty
    ma <|> mb = StateT $ (<|>) <$> runStateT ma <*> runStateT mb


instance (Monad m) => Monad (StateT s m) where
    ma >>= f = StateT $ \s -> do
            (s', a) <- runStateT ma s
            runStateT (f a) s'

instance MonadTrans (StateT s) where
    lift a = StateT $ \s -> (,) s <$> a

{- ArrowT -}

newtype ArrowT i m a = ArrowT {runArrowT :: i -> m a}

runArrowT' :: (Monad m) => m i -> ArrowT i m a -> m a
runArrowT' = flip $ (=<<) . runArrowT

instance (Functor f) => Functor (ArrowT i f) where
    fmap f (ArrowT ma) = ArrowT $ fmap f . ma

instance (Applicative f) => Applicative (ArrowT i f) where
    pure = ArrowT . const . pure
    (ArrowT mf) <*> (ArrowT ma) = ArrowT $ (<*>) <$> mf <*> ma

instance (Alternative f) => Alternative (ArrowT i f) where
    empty = ArrowT $ const empty
    (ArrowT ma) <|> (ArrowT mb) = ArrowT $ (<|>) <$> ma <*> mb

instance (Monad m) => Monad (ArrowT i m) where
    (ArrowT ma) >>= f = ArrowT $ \i -> do
        a <- ma i
        runArrowT (f a) i

instance MonadTrans (ArrowT i) where
    lift = ArrowT . const
