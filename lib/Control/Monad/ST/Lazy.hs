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
import Primitives(primPerformIO)
import Control.Monad.Fix
import Control.Monad.ST(RealWorld)
import qualified Control.Monad.ST_Type as S
import qualified Control.Monad.ST as S

primLazyBind :: IO a -> (a -> IO b) -> IO b
primLazyBind = _primitive "IO.lazyBind"
primStrictIO :: IO a -> IO a
primStrictIO = _primitive "IO.strict"

newtype ST s a = ST {unST :: S.ST s a}

-- This gets a type error when defined in terms of S.runST
runST :: (forall s . ST s a) -> a
runST = primPerformIO . S.unST . unST

-- XXX This seems suspicious; it's just the strict fix.
fixST :: (a -> ST s a) -> ST s a
fixST f = ST (S.fixST (unST . f))

strictToLazyST :: S.ST s a -> ST s a
strictToLazyST = ST . S.ST . primStrictIO . S.unST

-- Is this right?  Should we force the lazy state?
lazyToStrictST :: ST s a -> S.ST s a
lazyToStrictST = unST

instance Functor (ST s) where
  fmap f a = a >>= (return . f)  -- defined in terms on >>=, so it becomes lazy

instance Applicative (ST s) where
  pure = ST . pure
  (<*>) = ap                     -- defined in terms on >>=, so it becomes lazy

instance Monad (ST s) where
  ma >>= k = ST $ S.ST $ primLazyBind (S.unST (unST ma)) (S.unST . unST . k)

instance MonadFix (ST s) where
  mfix = fixST

---------------------------------
-- This does not belong here since it's GHC specific,
-- but to be compatible we do it the same way.

stToIO :: ST RealWorld a -> IO a
stToIO = S.unST . lazyToStrictST

