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
import qualified Prelude() -- do not import Prelude
import Control.Monad
import Data.Fractional
import Data.Function
import Data.Ratio_Type (Rational)
import Data.RealFloat (RealFloat)
import Data.RealFrac (RealFrac)
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
fromRat = fromRational
