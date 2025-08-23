module Numeric.Show(
  showSigned,
  showSignedNeg,
  showIntAtBase,
  showInt,
  showBin,
  showHex,
  showOct,
  showIntegral,
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
  | an < 0 =
    if -an < 0 then
      -- We are at minBound
      showPos (- quot an base) . showPos (- rem an base)
    else
      error "Numeric.showIntAtBase: negative argument"
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

showIntegral :: (Integral a) => Int -> a -> ShowS
showIntegral = showSigned showInt
