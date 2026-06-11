module Data.String.Interpolate where
import qualified Prelude()
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Double
import Data.Float
import Data.Function
import Data.Int
import Data.Integer
import Data.List
import Data.Monoid
import Data.Semigroup
import Data.String
import Data.Word
import {-# SOURCE #-} Data.Typeable
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

interpolateRaw :: String -> StringBuilder
interpolateRaw = fromString

interpolateValue :: Interpolate a => a -> StringBuilder
interpolateValue = interpolate

interpolateAppend :: StringBuilder -> StringBuilder -> StringBuilder
interpolateAppend = mappend

interpolateEmpty :: StringBuilder
interpolateEmpty = mempty

interpolateFinalize :: StringBuilder -> String
interpolateFinalize = buildString

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

instance Interpolate Double where
  interpolate = fromString . show

instance Interpolate Bool where
  interpolate = fromString . show
