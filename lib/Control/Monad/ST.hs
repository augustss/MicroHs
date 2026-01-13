module Control.Monad.ST(
  ST,
  runST,
  fixST,
  --
  RealWorld, stToIO,  -- GHC compat
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives(primPerformIO)
import Control.Monad.Fix
import Control.Monad.ST_Type
import System.IO(fixIO)

runST :: (forall s . ST s a) -> a
runST (ST ioa) = primPerformIO ioa

fixST :: (a -> ST s a) -> ST s a
fixST f = ST (fixIO (unST . f))

instance Functor (ST s) where
  fmap f (ST x) = ST (fmap f x)

instance Applicative (ST s) where
  pure x = ST (pure x)
  ST x <*> ST y = ST (x <*> y)

instance Monad (ST s) where
  ST x >>= f = ST (x >>= (unST . f))

instance MonadFix (ST s) where
  mfix = fixST

---------------------------------
-- This does not belong here since it's GHC specific,
-- but to be compatible we do it the same way.

data RealWorld  -- Just to be compatible with GHC.  We don't use it.

stToIO :: ST RealWorld a -> IO a
stToIO = unST
