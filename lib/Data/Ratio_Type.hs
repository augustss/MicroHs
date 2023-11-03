module Data.Ratio_Type(module Data.Ratio_Type) where
import Primitives
import Data.Integer_Type

data Ratio a = (:%) a a   -- XXX should be strict

type Rational = Ratio Integer

