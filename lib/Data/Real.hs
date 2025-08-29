module Data.Real(module Data.Real) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Num
import Data.Ord
import Data.Ratio_Type
import {-# SOURCE #-} Data.Typeable

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
