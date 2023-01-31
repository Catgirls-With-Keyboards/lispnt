{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Control.Trans (MonadTrans(..)) where
import Control.Applicative (Alternative(..))

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: (Monad m) => m a -> t m a

{- MaybeT -}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

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
            travbind f = (=<<) $ \case
                Nothing -> pure Nothing
                Just a -> f a

instance MonadTrans MaybeT where
    lift = MaybeT . (<$>) pure

{- EitherT -}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- swaps Left with Right
mirror :: (Functor m) => EitherT e m a -> EitherT a m e
mirror = EitherT . (<$>) inner . runEitherT
    where
        inner (Left e) = Right e
        inner (Right a) = Left a

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
            travbind f = (=<<) $ \case
                Left e -> pure (Left e)
                Right a -> f a

instance MonadTrans (EitherT e) where
    lift = EitherT . (<$>) pure

{- MeitherT -}

type MeitherT e = EitherT (Maybe e)

{- StateT -}

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- retrieves the 'background' value
get :: (Applicative m) => StateT s m s
get = StateT $ \s -> pure (s, s)

-- assigns to the 'background' value
set :: (Applicative m) => s -> StateT s m ()
set s = StateT $ \s' -> pure (s, ())

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
