-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(module Data.Double, Double) where
import Primitives
import Control.Error
import Data.Bool_Type
import Data.Eq
import Data.Floating
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

instance Floating Double where
  pi     = 3.141592653589793
  log  x = primPerformIO (clog x)
  exp  x = primPerformIO (cexp x)
  sqrt x = primPerformIO (csqrt x)
  sin  x = primPerformIO (csin x)
  cos  x = primPerformIO (ccos x)
  tan  x = primPerformIO (ctan x)
  asin x = primPerformIO (casin x)
  acos x = primPerformIO (cacos x)
  atan x = primPerformIO (catan x)

foreign import ccall "log"  clog  :: Double -> IO Double
foreign import ccall "exp"  cexp  :: Double -> IO Double
foreign import ccall "sqrt" csqrt :: Double -> IO Double
foreign import ccall "sin"  csin  :: Double -> IO Double
foreign import ccall "cos"  ccos  :: Double -> IO Double
foreign import ccall "tan"  ctan  :: Double -> IO Double
foreign import ccall "asin" casin :: Double -> IO Double
foreign import ccall "acos" cacos :: Double -> IO Double
foreign import ccall "atan" catan :: Double -> IO Double
