module Data.Ratio_Type(module Data.Ratio_Type) where
import Primitives
import Data.Integer_Type

data Ratio a = (:%) a a   -- XXX should be strict

type Rational = Ratio Integer

_integerToRational :: Integer -> Rational
_integerToRational x = x :% (1::Integer)
