module Control.Monad(
  Functor(..),
  Monad(..),
  MonadPlus(..),
  mapM,
  mapM_,
  forM,
  forM_,
  sequence,
  sequence_,
  (=<<),
  (>=>),
  (<=<),
  forever,
  void,
  join,
  msum,
  mfilter,
  filterM,
  mapAndUnzipM,
  zipWithM,
  zipWithM_,
  foldM,
  foldM_,
  replicateM,
  replicateM_,
  guard,
  when,
  unless,
  liftM,
  liftM2,
  liftM3,
  ap,
  -- XXX move
  partitionM,
  ) where
import Prelude()              -- do not import Prelude
import Primitives  -- for fixity
import Control.Applicative
import Control.Error
import Data.Bool
import Data.Char_Type
import Data.Function
import Data.Functor
import Data.List
import Data.Monoid
import Data.Ord
--import Data.Maybe

infixl 1 >>, >>=
infixr 1 <=<, >=>

class (Applicative m) => Monad m where
  (>>=)  :: forall a b . m a -> (a -> m b) -> m b
  (>>)   :: forall a b . m a -> m b -> m b
  ma >> mb = ma >>= \ _ -> mb

  -- Maybe remove this
  return :: forall a . a -> m a
  return = pure

-----

mapM :: forall m a b . Monad m => (a -> m b) -> [a] -> m [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> return []
        a : as -> do
          b <- f a
          bs <- rec as
          return (b : bs)
  in rec

mapM_ :: forall m a b . Monad m => (a -> m b) -> [a] -> m ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> return ()
        a : as -> do
          _ <- f a
          rec as
  in rec

forM :: forall m a b . Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

forM_ :: forall m a b . Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

sequence :: forall m a . Monad m => [m a] -> m [a]
sequence = mapM id

sequence_ :: forall m a . Monad m => [m a] -> m ()
sequence_ = mapM_ id

(=<<) :: forall m a b . Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(<=<) :: forall m a b c . Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \ a -> do
  b <- g a
  f b

(>=>) :: forall m a b c . Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = flip (<=<)

forever :: forall f a b . (Applicative f) => f a -> f b
forever a = let { a' = a *> a' } in a'

-----

join :: forall m a . (Monad m) => m (m a) -> m a
join x = x >>= id

filterM :: forall m a . Applicative m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x : xs) = liftA2 (\ flg -> if flg then (x:) else id) (p x) (filterM p xs)

-- XXX could relax some Monad to Applicative
mapAndUnzipM  :: forall m a b c . (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = unzip <$> mapM f xs

zipWithM :: forall m a b c . (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

zipWithM_ :: forall m a b c . (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

foldM :: forall m a b . (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM = foldlM

foldM_ :: forall m a b . (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
foldM_ f a xs  = foldlM f a xs >> return ()

foldlM :: forall m a b . (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ z [] = pure z
foldlM f z (x : xs) = do
  z' <- f z x
  foldlM f z' xs

replicateM  :: forall m a . (Applicative m) => Int -> m a -> m [a]
replicateM cnt0 f = loop cnt0
  where
    loop cnt =
      if cnt <= (0::Int) then pure []
      else liftA2 (:) f (loop (cnt `primIntSub` 1))

replicateM_  :: forall m a . (Applicative m) => Int -> m a -> m ()
replicateM_ cnt0 f = loop cnt0
  where
    loop cnt =
      if cnt <= (0::Int) then pure ()
      else f *> (loop (cnt `primIntSub` 1))

-----

when :: forall m . Applicative m => Bool -> m () -> m ()
when p ma = if p then ma else pure ()

unless :: forall m . Applicative m => Bool -> m () -> m ()
unless p ma = if p then pure () else ma

-----

liftM :: forall m r a1 . (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1 = do { x1 <- m1; return (f x1) }
liftM2 :: forall m r a1 a2 . (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
liftM3 :: forall m r a1 a2 a3 . (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3 = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

ap :: forall m a b . Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

-----

partitionM :: forall m a . Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  b <- p x
  (ts,fs) <- partitionM p xs
  return $ if b then (x:ts, fs) else (ts, x:fs)

instance forall a . Functor ((->) a) where
  fmap = (.)

instance forall a . Applicative ((->) a) where
  pure = const
  f <*> g = \ a -> f a (g a)

instance forall a . Monad ((->) a) where
  x >>= y = \ z -> y (x z) z

instance Monad Dual where
  m >>= k = k (getDual m)

instance Monad [] where
  (>>=) = flip concatMap

{-
-- Same for Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure a = Just a
  (<*>) = ap

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a  >>= f = f a
-}

class (Monad m) => MonadPlus m where
  mzero :: forall a . m a
  mplus :: forall a . m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

msum :: forall m a . (MonadPlus m) => [m a] -> m a
msum [] = mzero
msum (ma:mas) = ma `mplus` msum mas

mfilter :: forall m a . (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter p ma = do
  a <- ma
  if p a then return a else mzero
