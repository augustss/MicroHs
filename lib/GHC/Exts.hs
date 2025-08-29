module GHC.Exts(
  stToIO, RealWorld,
  build,
  ) where
import Control.Monad.ST_Type

build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs
