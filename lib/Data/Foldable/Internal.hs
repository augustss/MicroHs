module Data.Foldable.Internal(module Data.Foldable.Internal) where
import qualified Prelude()
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Function
import Data.Functor
import Data.Maybe_Type
import Data.Monoid.Internal hiding (Max(..), Min(..))
import Data.List
import Data.Ord
import {-# SOURCE #-} Data.Typeable

newtype Max a = Max { getMax :: Maybe a }

newtype Min a = Min { getMin :: Maybe a }

instance forall a . Ord a => Semigroup (Max a) where
    m <> Max Nothing = m
    Max Nothing <> n = n
    (Max m@(Just x)) <> (Max n@(Just y))
      | x >= y    = Max m
      | otherwise = Max n

instance forall a . Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mconcat = foldl' (<>) mempty

instance forall a . Ord a => Semigroup (Min a) where
    m <> Min Nothing = m
    Min Nothing <> n = n
    (Min m@(Just x)) <> (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n

instance forall a . Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mconcat = foldl' (<>) mempty

newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateL s) where
    pure x = StateL (\ s -> (s, x))
    StateL kf <*> StateL kv = StateL $ \ s ->
        let (s', f) = kf s
            (s'', v) = kv s'
        in (s'', f v)

newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
    fmap f (StateR k) = StateR $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateR s) where
    pure x = StateR (\ s -> (s, x))
    StateR kf <*> StateR kv = StateR $ \ s ->
        let (s', v) = kv s
            (s'', f) = kf s'
        in (s'', f v)

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)

instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- runStateT m s
        runStateT (k a) s'
