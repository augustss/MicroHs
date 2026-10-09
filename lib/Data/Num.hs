-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Num(module Data.Num) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Integer_Type
import {-# SOURCE #-} Data.Typeable

infixl 6 +,-
infixl 7 *

class NumAdd a where
  (+) :: a -> a -> a

class (NumAdd a, NumLit a) => NumSub a where
  negate :: a -> a
  (-) :: a -> a -> a
  negate x = 0 - x
  x - y = x + negate y

class NumMul a where
  (*) :: a -> a -> a

class NumAbs a where
  abs :: a -> a
  signum :: a -> a

class NumLit a where
  fromInteger :: Integer -> a

type Num a = (NumAdd a, NumSub a, NumMul a, NumAbs a, NumLit a)

subtract :: forall a . Num a => a -> a -> a
subtract x y = y - x
