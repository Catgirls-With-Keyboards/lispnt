module Control.Varients where
import Control.Trans
import Control.Applicative (Alternative(..))

type MemoT k v m a = StateT (ArrowT k m v) m a

learn :: (Monad m, Alternative m) => (k -> m v) -> MemoT k v m ()
learn f = do
    state <- get
    set $ state <|> ArrowT f

memorise :: (Monad m, Alternative m, Eq k) => k -> m v -> MemoT k v m ()
memorise k mv = learn update
    where
        update k' = if k' == k then mv else empty

recall :: (Monad m, Alternative m) => k -> MemoT k v m v
recall k = do
    state <- get
    lift $ runArrowT' (pure k) state

memoCall :: (Monad m, Alternative m, Eq k) => k -> (k -> m v) -> MemoT k v m v
memoCall k f = recall k <|> do
    memorise k (f k)
    recall k