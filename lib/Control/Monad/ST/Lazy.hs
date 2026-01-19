module Control.Monad.ST.Lazy(
  ST,
  runST,
  fixST,
  strictToLazyST,
  lazyToStrictST,
  --
  RealWorld, stToIO,  -- GHC compat
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Monad.Fix
import Control.Monad.ST(RealWorld)
import qualified Control.Monad.ST_Type as S
import System.IO.Unsafe(unsafeInterleaveIO, unsafePerformIO)

newtype ST s a = ST { unST :: State s -> (a, State s) }

data State s = S

runST :: (forall s . ST s a) -> a
runST (ST st) = case st S of (r, _) -> r

fixST :: (a -> ST s a) -> ST s a
fixST f = ST $ \s -> let q@(r, _s') = unST (f r) s in q

strictToLazyST :: S.ST s a -> ST s a
strictToLazyST (S.ST io) = ST $ \s -> s `seq`
  case unsafePerformIO (fmap MkSolo io) of
    MkSolo a -> (a, s)

lazyToStrictST :: ST s a -> S.ST s a
lazyToStrictST (ST st) = S.ST $ fmap id (case st S of (x, _) -> pure x)

instance Functor (ST s) where
  fmap f m = ST $ \s ->
    let (r, s') = unST m s
    in (f r, s')

instance Applicative (ST s) where
  pure x = ST $ \s -> (x, s)
  liftA2 f m n = ST $ \s ->
    let
      (x, s') = unST m s
      (y, s'') = unST n s'
    in (f x y, s'')

instance Monad (ST s) where
  m >>= k = ST $ \s ->
    let (r, s') = unST m s
    in unST (k r) s'

instance MonadFix (ST s) where
  mfix = fixST

---------------------------------
-- This does not belong here since it's GHC specific,
-- but to be compatible we do it the same way.

stToIO :: ST RealWorld a -> IO a
stToIO = S.unST . lazyToStrictST
