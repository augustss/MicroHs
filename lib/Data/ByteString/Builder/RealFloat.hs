module Data.ByteString.Builder.RealFloat where

import qualified Prelude ()
import MiniPrelude

import Data.ByteString.Builder.Internal
import Data.Double
import Data.Float
import Numeric.FormatFloat

floatDec :: Float -> Builder
floatDec = formatFloat generic

doubleDec :: Double -> Builder
doubleDec = formatDouble generic

formatFloat :: FloatFormat -> Float -> Builder
formatFloat (MkFloatFormat fmt prec) f =
  stringUtf8 $ case fmt of
    FScientific -> showEFloat prec f ""
    FStandard -> showFFloat prec f ""
    FGeneric -> showGFloat prec f ""

formatDouble :: FloatFormat -> Double -> Builder
formatDouble (MkFloatFormat fmt prec) d =
  stringUtf8 $ case fmt of
    FScientific -> showEFloat prec d ""
    FStandard -> showFFloat prec d ""
    FGeneric -> showGFloat prec d ""

data FloatFormat = MkFloatFormat FormatMode (Maybe Int)

standard :: Int -> FloatFormat
standard n = MkFloatFormat FStandard (Just n)

standardDefaultPrecision :: FloatFormat
standardDefaultPrecision = MkFloatFormat FStandard Nothing

scientific :: FloatFormat
scientific = MkFloatFormat FScientific Nothing

generic :: FloatFormat
generic = MkFloatFormat FGeneric Nothing

data FormatMode
  = FScientific     -- ^ scientific notation
  | FStandard       -- ^ standard notation with `Maybe Int` digits after the decimal
  | FGeneric        -- ^ dispatches to scientific or standard notation based on the exponent
  deriving Show
