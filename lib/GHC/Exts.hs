module GHC.Exts(
  unsafeIOToST, stToIO, RealWorld,
  build, augment,
  inline,
  ) where
import Control.Monad.ST_Type

build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs

----

data RealWorld  -- Just to be compatible with GHC.  We don't use it.

stToIO :: forall a . ST RealWorld a -> IO a
stToIO = unST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = ST

inline :: a -> a
inline x = x
