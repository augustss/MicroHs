-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(module Data.Double, Double) where
import Primitives
import Control.Error
import Data.Bool_Type
import Data.Eq
import Data.Fractional
import Data.Integer
import Data.Ord
import Data.Ratio
import Data.Num
import Text.Show

instance Num Double where
  (+)  = primDoubleAdd
  (-)  = primDoubleSub
  (*)  = primDoubleMul
  abs x = if x < 0.0 then negate x else x
  signum x =
    case compare x 0.0 of
      LT -> -1.0
      EQ ->  0.0
      GT ->  1.0
  fromInteger = _integerToDouble

instance Fractional Double where
  (/) = primDoubleDiv
  -- This version of fromRational can go horribly wrong
  -- if the integers are bigger than can be represented in a Double.
  -- It'll do for now.
  fromRational x = fromInteger (numerator x) `primDoubleDiv` fromInteger (denominator x)

instance Eq Double where
  (==) = primDoubleEQ
  (/=) = primDoubleNE

instance Ord Double where
  (<)  = primDoubleLT
  (<=) = primDoubleLE
  (>)  = primDoubleGT
  (>=) = primDoubleGE
  
-- | this primitive will print doubles with up to 6 decimal points
-- it turns out that doubles are extremely tricky, and just printing them is a
-- herculean task of its own...
instance Show Double where
  show = primDoubleShow
