module Data.Real(module Data.Real) where
import Primitives
import Data.Fractional
import Data.Ratio_Type

class Real a where
  toRational :: a -> Rational

realToFrac :: forall a b . (Real a, Fractional b) => a -> b
realToFrac a = fromRational (toRational a)
