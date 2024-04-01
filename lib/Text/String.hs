-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Text.String(module Text.String) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Fractional
import Data.Function
import Data.Int
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe
import Data.Num
import Data.Ord
import Data.Ratio
import Data.Real
import Data.String
import Data.Tuple
import Text.Read
import Text.Show

showPairS :: forall a b . (a -> String) -> (b -> String) -> (a, b) -> String
showPairS sa sb (a, b) = "(" ++ sa a ++ "," ++ sb b ++ ")"
