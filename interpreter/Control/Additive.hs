module Control.Additive (Additive(..)) where
import Control.Applicative (Alternative)

{- Additive -}

class (Alternative f) => Additive f where
    -- (ma <+> mb) >>= const empty = (ma >>= const empty) <|> (mb >>= const empty)
    infixl 4 <+>
    (<+>) :: f (a -> b) -> f a -> f b

    infixl 4 <+
    (<+) :: f a -> f b -> f a
    ma <+ mb = const <$> ma <+> mb

    infixl 4 +>
    (+>) :: f a -> f b -> f b
    ma +> mb = final <$> ma <+> mb
        where
            final = const id