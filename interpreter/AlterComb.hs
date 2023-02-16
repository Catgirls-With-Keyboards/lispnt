{-# LANGUAGE TupleSections #-}
module AlterComb where
import Control.Applicative (Alternative(..))

mostTill :: (Alternative f) => f a -> f b -> f ([a], b)
mostTill ma mb = (into <$> ma <*> mostTill ma mb) <|> (([],) <$> mb)
    where
        into a (as, b) = (a:as, b)

leastTill :: (Alternative f) => f a -> f b -> f ([a], b)
leastTill ma mb = (([],) <$> mb) <|> (into <$> ma <*> leastTill ma mb)
    where
        into a (as, b) = (a:as, b)

most :: (Alternative f) => f a -> f [a]
most ma = fst <$> mostTill ma (pure ())

most1Till :: (Alternative f) => f a -> f b -> f ([a], b)
most1Till ma mb = into <$> ma <*> mostTill ma mb
    where
        into a (as, b) = (a:as, b)

least1Till :: (Alternative f) => f a -> f b -> f ([a], b)
least1Till ma mb = into <$> ma <*> leastTill ma mb
    where
        into a (as, b) = (a:as, b)

most1 :: (Alternative f) => f a -> f [a]
most1 ma = fst <$> most1Till ma (pure ())

mostInterlaceTill :: (Alternative f) => f a -> f b -> f c -> f (Either c ([(a, b)], (a, c)))
mostInterlaceTill ma mb mc = (Right <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)) <|> (Left <$> mc)

leastInterlaceTill :: (Alternative f) => f a -> f b -> f c -> f (Either c ([(a, b)], (a, c)))
leastInterlaceTill ma mb mc = (Left <$> mc) <|> (Right <$> mostTill ((,) <$> ma <*> mb) ((,) <$> ma <*> mc))

mostInterlace :: (Alternative f) => f a -> f b -> f (Maybe ([(a, b)], a))
mostInterlace ma mb = simplify <$> mostInterlaceTill ma mb (pure ())
    where
        simplify (Left ()) = Nothing
        simplify (Right (abs, (a, ()))) = Just (abs, a)

most1InterlaceTill :: (Alternative f) => f a -> f b -> f c -> f (Either c ([(a, b)], (a, c)))
most1InterlaceTill ma mb mc = (Right <$> most1Till ((,) <$> ma <*> mb) ((,) <$> ma <*> mc)) <|> (Left <$> mc)

least1InterlaceTill :: (Alternative f) => f a -> f b -> f c -> f (Either c ([(a, b)], (a, c)))
least1InterlaceTill ma mb mc = (Left <$> mc) <|> (Right <$> most1Till ((,) <$> ma <*> mb) ((,) <$> ma <*> mc))

most1Interlace :: (Alternative f) => f a -> f b -> f (Maybe ([(a, b)], a))
most1Interlace ma mb = simplify <$> most1InterlaceTill ma mb (pure ())
    where
        simplify (Left ()) = Nothing
        simplify (Right (abs, (a, ()))) = Just (abs, a)

mostPaddedTill :: (Alternative f) => f a -> f b -> f c -> f ([a], c)
mostPaddedTill ma mb mc = simplify <$> mostInterlaceTill ma mb mc
    where
        simplify (Left c) = ([], c)
        simplify (Right (abs, (a, c))) = (fmap fst abs ++ [a], c)

leastPaddedTill :: (Alternative f) => f a -> f b -> f c -> f ([a], c)
leastPaddedTill ma mb mc = simplify <$> leastInterlaceTill ma mb mc
    where
        simplify (Left c) = ([], c)
        simplify (Right (abs, (a, c))) = (fmap fst abs ++ [a], c)

mostPadded :: (Alternative f) => f a -> f b -> f [a]
mostPadded ma mb = simplify <$> mostInterlace ma mb
    where
        simplify Nothing = []
        simplify (Just (as, a)) = fmap fst as ++ [a]

most1PaddedTill :: (Alternative f) => f a -> f b -> f c -> f ([a], c)
most1PaddedTill ma mb mc = simplify <$> most1InterlaceTill ma mb mc
    where
        simplify (Left c) = ([], c)
        simplify (Right (abs, (a, c))) = (fmap fst abs ++ [a], c)

least1PaddedTill :: (Alternative f) => f a -> f b -> f c -> f ([a], c)
least1PaddedTill ma mb mc = simplify <$> least1InterlaceTill ma mb mc
    where
        simplify (Left c) = ([], c)
        simplify (Right (abs, (a, c))) = (fmap fst abs ++ [a], c)

most1Padded :: (Alternative f) => f a -> f b -> f [a]
most1Padded ma mb = simplify <$> most1Interlace ma mb
    where
        simplify Nothing = []
        simplify (Just (as, a)) = fmap fst as ++ [a]
