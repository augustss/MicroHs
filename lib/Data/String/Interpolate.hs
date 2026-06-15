module Data.String.Interpolate(
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,
  StringBuilder,
  buildString,
  Interpolate(..),
  --
  Adjust(..),
  FloatFmt(..),
  ) where
import qualified Prelude()
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Double
import Data.Eq
import Data.Float
import Data.Function
import Data.Int
import Data.Integer
import Data.List
import Data.Num
import Data.Maybe
import Data.Monoid
import Data.Monoid.Internal(Semigroup(..))
import Data.Ord
import Data.RealFloat
import Data.String
import Data.Word
import {-# SOURCE #-} Data.Typeable
import Numeric.FormatFloat
import Text.Show

-----
-- Implementation of s"..."
--
-- Example:
--   s"foo ${f a b} bar ${g x} baz ${name}"
-- desugared
--   interpolateFinalize $
--   interpolateRaw "foo "   `interpolateAppend`
--   interpolateValue (f a b)`interpolateAppend`
--   interpolateRaw " bar "  `interpolateAppend`
--   interpolateValue (g x)  `interpolateAppend`
--   interpolateRaw " baz "  `interpolateAppend`
--   interpolateValue name   `interpolateAppend`
--   interpolateEmpty
--

interpolateRaw :: String -> StringBuilder
interpolateRaw = fromString

interpolateValue :: Interpolate a => a -> StringBuilder
interpolateValue = interpolate

interpolateAppend :: StringBuilder -> StringBuilder -> StringBuilder
interpolateAppend = mappend

interpolateEmpty :: StringBuilder
interpolateEmpty = mempty

interpolateFinalize :: IsString a => StringBuilder -> a
interpolateFinalize = fromString . buildString

-----
-- StringBuilder

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)

instance IsString StringBuilder where
  fromString s = StringBuilder (Endo (s <>))

buildString :: StringBuilder -> String
buildString (StringBuilder (Endo f)) = f ""

-----
-- Interpolation of values

class Interpolate a where
  interpolate :: (IsString s, Monoid s) => a -> s

instance Interpolate String where
  interpolate = fromString
instance Interpolate Char where
  interpolate c = fromString [c]

instance Interpolate Integer where
  interpolate = fromString . show
instance Interpolate Int where
  interpolate = fromString . show
instance Interpolate Int8 where
  interpolate = fromString . show
instance Interpolate Int16 where
  interpolate = fromString . show
instance Interpolate Int32 where
  interpolate = fromString . show
instance Interpolate Int64 where
  interpolate = fromString . show

instance Interpolate Word where
  interpolate = fromString . show
instance Interpolate Word8 where
  interpolate = fromString . show
instance Interpolate Word16 where
  interpolate = fromString . show
instance Interpolate Word32 where
  interpolate = fromString . show
instance Interpolate Word64 where
  interpolate = fromString . show

instance Interpolate Float where
  interpolate = fromString . show
instance Interpolate Double where
  interpolate = fromString . show

instance Interpolate Bool where
  interpolate = fromString . show

----------------------------------
-- MicroHs externsions

measure :: Interpolate a => a -> Int
measure = length . buildString . interpolate

data Adjust a = LeftAdj Int a | RightAdj Int a
  deriving (Eq, Show)

instance Interpolate a => Interpolate (Adjust a) where
  interpolate (LeftAdj  n a) = interpolate a <> fromString (replicate (n - measure a) ' ')
  interpolate (RightAdj n a) = fromString (replicate (n - measure a) ' ') <> interpolate a

data FloatFmt a = EFloat Int a | FFloat Int a | GFloat Int a
  deriving (Eq, Show)

instance (RealFloat a) => Interpolate (FloatFmt a) where
  interpolate (EFloat n a) = fromString (showEFloat (Just n) a "")
  interpolate (FFloat n a) = fromString (showFFloat (Just n) a "")
  interpolate (GFloat n a) = fromString (showGFloat (Just n) a "")
