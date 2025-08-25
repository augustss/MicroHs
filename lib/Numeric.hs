module Numeric(
  showSigned,
  showIntAtBase,
  showInt,
  showBin,
  showHex,
  showOct,
  showEFloat,--
  showFFloat,
  showGFloat,
  showFloat,
  floatToDigits,
  readSigned,
  readInt,
  readBin,
  readDec,
  readOct,
  readHex,
  readFloat,
  lexDigits,
  fromRat,
  ) where

import Primitives
import Data.Integer_Type
import Data.Integer.Internal
import Data.Ratio_Type
import Data.Ratio
import Numeric.FormatFloat
import Numeric.Read
import Numeric.Show
import Text.ParserCombinators.ReadP (ReadP, pfail, readP_to_S)
import Text.Read.Internal (ReadS, lexDigits)
import qualified Text.Read.Lex as L

-- | Reads an /unsigned/ 'RealFrac' value,
-- expressed in decimal scientific notation.
readFloat :: RealFrac a => ReadS a
readFloat = readP_to_S readFloatP

readFloatP :: RealFrac a => ReadP a
readFloatP =
  do tok <- L.lex
     case tok of
       L.Number n -> return $ fromRational $ L.numberToRational n
       _          -> pfail

-- | Converts a 'Rational' value into any type in class 'RealFloat'.
fromRat :: (RealFloat a) => Rational -> a

-- Deal with special cases first, delegating the real work to fromRat'
fromRat (n :% 0) | n > 0     =   1/0       -- +Infinity
                 | n < 0     = -(1/0)      -- -Infinity
                 | otherwise =   0/0       -- NaN

fromRat (n :% d) | n > 0     = fromRat' (n :% d)
                 | n < 0     = - fromRat' ((-n) :% d)
                 | otherwise = encodeFloat 0 0             -- Zero

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.

fromRat' :: (RealFloat a) => Rational -> a
-- Invariant: argument is strictly positive
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
        (minExp0, _) = floatRange r
        minExp = minExp0 - p            -- the real minimum exponent
        xMax   = toRational (b ^ p)
        ln     = integerLogBase b (numerator x)
        ld     = integerLogBase b (denominator x)
        p0     = (ln - ld - p) `max` minExp
        -- if x = n/d and ln = integerLogBase b n, ld = integerLogBase b d,
        -- then b^(ln-ld-1) < x < b^(ln-ld+1)
        f = if p0 < 0 then 1 :% (b ^ (-p0)) else (b ^ p0) :% 1
        x0 = x / f
        -- if ln - ld >= minExp0, then b^(p-1) < x0 < b^(p+1), so there's at most
        -- one scaling step needed, otherwise, x0 < b^p and no scaling is needed
        (x', p') = if x0 >= xMax then (x0 / toRational b, p0+1) else (x0, p0)
        r = encodeFloat (round x') p'

wordLog2 :: Word -> Int
wordLog2 w = _wordSize - 1 - primWordClz w

integerLog2 :: Integer -> Int
integerLog2 (I sign ds) =
  case sign of
    Minus -> 0 :: Int
    Plus -> go ds 0
  where
    go [] _ = 0 :: Int
    go [d] i = wordLog2 d + (i `primIntShl` shiftD)
    go (d : ds) i = go ds (i + 1)

integerLogBase :: Integer -> Integer -> Int
integerLogBase b i
  | i == 0 = 0 :: Int
  | b < 2 = error "invalid base"
  | b == 2 = integerLog2 i
  | otherwise = case go b of (_, e) -> e
  where
    go p = if i < p
      then (i, 0)
      else case go (p * p) of
        (q, e) -> if q < p
          then (q, 2 * e)
          else (q `quot` p, 2 * e + 1)
