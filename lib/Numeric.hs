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
import qualified Prelude(); import MiniPrelude
import Data.Fractional
import Data.Integer_Type
import Data.Ratio
import Data.Ratio_Type
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Math.NumberTheory.Logarithms
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

fromRat' :: (RealFloat a) => Rational -> a
-- Invariant: argument is strictly positive
fromRat' x = r
  where
    n = numerator x
    d = denominator x
    b = floatRadix r
    p = floatDigits r
    (minExp, _) = floatRange r
    maxVal = b ^ p
    r =
      if n < maxVal && d < maxVal then
        -- no scaling needed
        fromInteger n / fromInteger d
      else
        let
          ln = integerLogBase b n
          ld = integerLogBase b d
          -- b^(ln-ld-1) < x < b^(ln-ld+1)
          p0 = max (ln - ld) minExp - p
          f = if p0 < 0 then 1 :% (b ^ (-p0)) else (b ^ p0) :% 1
          x0 = x / f
          -- if ln - ld >= minExp, then b^(p-1) < x0 < b^(p+1), so there's at most
          -- one scaling step needed, otherwise, x0 < b^p and no scaling is needed
          (x', p') = if x0 >= toRational maxVal then (x0 / toRational b, p0 + 1) else (x0, p0)
        in encodeFloat (round x') p'
