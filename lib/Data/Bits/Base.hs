module Data.Bits.Base(module Data.Bits.Base) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Eq
import Data.Function
import Data.Int.Int
import Data.Integral
import Data.Maybe
import Data.Ord
import Data.Num
import {-# SOURCE #-} Data.Typeable
import Text.Show

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

class Eq a => Bits a where
  (.&.)             :: a -> a -> a
  (.|.)             :: a -> a -> a
  xor               :: a -> a -> a
  complement        :: a -> a
  shift             :: a -> Int -> a
  rotate            :: a -> Int -> a
  zeroBits          :: a
  bit               :: Int -> a
  setBit            :: a -> Int -> a
  clearBit          :: a -> Int -> a
  complementBit     :: a -> Int -> a
  testBit           :: a -> Int -> Bool
  shiftL            :: a -> Int -> a
  unsafeShiftL      :: a -> Int -> a
  shiftR            :: a -> Int -> a
  unsafeShiftR      :: a -> Int -> a
  rotateL           :: a -> Int -> a
  rotateR           :: a -> Int -> a
  popCount          :: a -> Int
  bitSizeMaybe      :: a -> Maybe Int
  bitSize           :: a -> Int
  isSigned          :: a -> Bool

  x `shift`   i | i<0       = x `shiftR` (- i)
                | i>0       = x `shiftL` i
                | otherwise = x


  x `rotate`  i | i<0       = x `rotateR` (- i)
                | i>0       = x `rotateL` i
                | otherwise = x

  {-
  x `rotate`  i | i<0 && isSigned x && x<0
                       = let left = i+bitSize x in
                         ((x `shift` i) .&. complement ((-1) `shift` left))
                         .|. (x `shift` left)
                | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
                | i==0 = x
                | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))
  -}

  zeroBits            = clearBit (bit 0) 0
  bitSize b           = fromMaybe (error "bitSize is undefined") (bitSizeMaybe b)
  x `setBit` i        = x .|. bit i
  x `clearBit` i      = x .&. complement (bit i)
  x `complementBit` i = x `xor` bit i

  x `shiftL`  i       = x `shift`  i
  x `unsafeShiftL` i  = x `shiftL` i
  x `shiftR`  i       = x `shift`  (- i)
  x `unsafeShiftR` i  = x `shiftR` i

  x `rotateL` i       = x `rotate` i

  x `rotateR` i       = x `rotate` (- i)


class Bits b => FiniteBits b where
  finiteBitSize :: b -> Int
  countLeadingZeros :: b -> Int
  countTrailingZeros :: b -> Int

  countLeadingZeros x = (w - 1) - go (w - 1)
    where
      go i | i < 0       = i -- no bit set
           | testBit x i = i
           | otherwise   = go (i - 1)

      w = finiteBitSize x

  countTrailingZeros x = go 0
    where
      go i | i >= w      = i
           | testBit x i = i
           | otherwise   = go (i + 1)

      w = finiteBitSize x

bitDefault :: (Bits a, Num a) => Int -> a
bitDefault i = 1 `shiftL` i

testBitDefault :: (Bits a, Num a) => a -> Int -> Bool
testBitDefault x i = (x .&. bit i) /= 0

popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
  where
    go c 0 = c
    go c w = go (c + 1) (w .&. (w - 1)) -- clear the least significant bit

_overflowError :: a
_overflowError = error "arithmetic overflow"

instance Bits Int where
  (.&.) = primIntAnd
  (.|.) = primIntOr
  xor   = primIntXor
  complement = primIntInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= _wordSize = 0
    | otherwise = x `primIntShl` i
  unsafeShiftL = primIntShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= _wordSize = 0
    | otherwise = x `primIntShr` i
  unsafeShiftR = primIntShr
  bitSizeMaybe _ = Just _wordSize
  bitSize _ = _wordSize
  bit = bitDefault
  testBit = testBitDefault
  popCount = primIntPopcount
  zeroBits = 0
  isSigned _ = True

instance FiniteBits Int where
  finiteBitSize _ = _wordSize
  countLeadingZeros = primIntClz
  countTrailingZeros = primIntCtz

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x
  | maybe True (<= x) yMinBound
  , maybe True (x <=) yMaxBound = Just y
  | otherwise                   = Nothing
  where
    y = fromIntegral x

    xWidth = bitSizeMaybe x
    yWidth = bitSizeMaybe y

    yMinBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y) = Just 0
      | isSigned x, isSigned y
      , Just yW <- yWidth = Just (negate $ bit (yW-1))
      | otherwise = Nothing

    yMaxBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y)
      , Just xW <- xWidth, Just yW <- yWidth
      , xW <= yW+1 = Nothing
      | Just yW <- yWidth = if isSigned y
                            then Just (bit (yW-1)-1)
                            else Just (bit yW-1)
      | otherwise = Nothing

isBitSubType :: (Bits a, Bits b) => a -> b -> Bool
isBitSubType x y
  -- Reflexive
  | xWidth == yWidth, xSigned == ySigned = True

  -- Every integer is a subset of 'Integer'
  | ySigned, isNothing yWidth                  = True
  | not xSigned, not ySigned, isNothing yWidth = True

  -- Sub-type relations between fixed-with types
  | xSigned == ySigned,   Just xW <- xWidth, Just yW <- yWidth = xW <= yW
  | not xSigned, ySigned, Just xW <- xWidth, Just yW <- yWidth = xW <  yW

  | otherwise = False
  where
    xWidth  = bitSizeMaybe x
    xSigned = isSigned     x

    yWidth  = bitSizeMaybe y
    ySigned = isSigned     y

