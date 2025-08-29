module Data.Bounded(module Data.Bounded) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import {-# SOURCE #-} Data.Typeable

class Bounded a where
  minBound :: a
  maxBound :: a
