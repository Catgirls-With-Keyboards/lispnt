module Data.Collections where
import qualified Data.Set (toList, fromList)

data Labeled l a = Labeled { label :: l, value :: a } deriving (Show)

instance (Eq l) => Eq (Labeled l a) where
    kva == kvb = label kva == label kvb

instance (Ord l) => Ord (Labeled l a) where
    kva >= kvb = label kva >= label kvb
    kva <= kvb = label kva <= label kvb

instance Functor (Labeled l) where
    fmap f la = la { value = f . value $ la }

newtype Record l a = Record { pairs :: [Labeled l a] }

record :: l -> a -> Record l a
record l a = Record [Labeled l a]

instance (Show l, Show a) => Show (Record l a) where
    show = show . pairs

instance (Ord l) => Functor (Record l) where
    fmap f (Record as) = Record . reduce $ fmap f <$> as
        where
            reduce = reverse . Data.Set.toList . Data.Set.fromList . reverse

instance (Ord l) => Semigroup (Record l a) where
    (Record as) <> (Record bs) = Record . reduce $ as <> bs
        where
            reduce = reverse . Data.Set.toList . Data.Set.fromList . reverse

instance (Ord l) => Monoid (Record l a) where
    mempty = Record []

newtype LeftBox a = LeftBox { contents :: a }

instance Semigroup (LeftBox a) where
    a <> _ = a
