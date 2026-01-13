module Control.Monad.ST.Lazy.Unsafe(
  unsafeInterleaveST,
  unsafeIOToST,
  ) where
import qualified Prelude();
import Primitives(IO)
import qualified Control.Monad.ST.Unsafe as S
import Control.Monad.ST.Lazy
import Data.Function

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . S.unsafeInterleaveST . lazyToStrictST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = strictToLazyST . S.unsafeIOToST
