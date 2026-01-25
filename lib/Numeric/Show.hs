module Numeric.Show(
  showSigned,
  showSignedNeg,
  showIntAtBase,
  showInt,
  showBin,
  showHex,
  showOct,
  showSignedInt,
  showUnsignedInt,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.Integral
import Data.List
import Data.Num
import Data.Ord
import Data.Real
import Text.Show(ShowS, showChar)

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned = showSignedNeg (< 0)

showSignedNeg :: (Real a) => (a -> Bool) -> (a -> ShowS) -> Int -> a -> ShowS
showSignedNeg isNeg showPos p n r
    | isNeg n =
      if p > (6::Int) then
        '(' : '-' : showPos (-n) (')' : r)
      else
        '-' : showPos (-n) r
    | otherwise = showPos n r

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
-- If the argument, n, is <0 and -n == n (i.e., n == minBound) it will
-- return the string for (abs n).
showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr an
  | base <= 1 = error "Numeric.showIntAtBase: unsupported base"
  | an < 0 = error "Numeric.showIntAtBase: negative argument"
  | otherwise = showPos an
   where
    showPos n r =
      let
        c = toChr (fromIntegral (rem n base))
      in  c `seq`
          if n < base then
            c : r
          else
            showPos (quot n base) (c : r)

showInt :: (Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showHex :: (Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: (Integral a) => a -> ShowS
showOct = showIntAtBase 8  intToDigit

showBin :: (Integral a) => a -> ShowS
showBin = showIntAtBase 2  intToDigit

showSignedInt :: (Integral a) => Int -> a -> ShowS
showSignedInt p n r
  | n < 0 =
    if p > (6::Int) then
      '(' : '-' : showUnsignedNeg n (')' : r)
    else
      '-' : showUnsignedNeg n r
  | otherwise = showUnsignedNeg (-n) r

showUnsignedNeg :: (Integral a) => a -> ShowS
showUnsignedNeg n r =
  let c = primChr (primOrd '0' - fromIntegral (rem n 10))
  in  if n > -10 then
        c : r
      else
        showUnsignedNeg (quot n 10) (c : r)

showUnsignedInt :: (Integral a) => Int -> a -> ShowS
showUnsignedInt _ = showInt
