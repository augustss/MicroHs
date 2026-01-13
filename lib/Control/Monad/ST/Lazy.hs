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
import Primitives(primPerformIO, primReturn)
import Control.Monad.Fix
import Control.Monad.ST(RealWorld)
import qualified Control.Monad.ST_Type as S

newtype ST s a = ST (IO a)

unST :: ST s a -> IO a
unST (ST io) = io

runST :: (forall s . ST s a) -> a
runST (ST ioa) = primPerformIO ioa

fixST :: (a -> ST s a) -> ST s a
fixST f = ST (let x = primPerformIO (unST (f x)) in primReturn x)

strictToLazyST :: S.ST s a -> ST s a
strictToLazyST (S.ST io) = ST io

lazyToStrictST :: ST s a -> S.ST s a
lazyToStrictST (ST io) = S.ST io

instance Functor (ST s) where
  fmap f (ST x) = ST (primReturn (f (primPerformIO x)))

instance Applicative (ST s) where
  pure x = ST (primReturn x)
  ST f <*> ST x = ST (primReturn (primPerformIO f (primPerformIO x)))

instance Monad (ST s) where
  ST x >>= f = f (primPerformIO x)

instance MonadFix (ST s) where
  mfix = fixST

---------------------------------
-- This does not belong here since it's GHC specific,
-- but to be compatible we do it the same way.

stToIO :: ST RealWorld a -> IO a
stToIO = unST
