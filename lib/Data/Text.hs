module Data.Text(
  Text,
  pack, unpack,
  empty,
  append,
  ) where
import Data.Monoid
import Data.Semigroup
import Data.String
import qualified Data.ByteString as BS

newtype Text = T BS.ByteString

instance Eq Text where
  (==) = cmp (==)
  (/=) = cmp (/=)

instance Ord Text where
  (<)  = cmp (<)
  (<=) = cmp (<=)
  (>)  = cmp (>)
  (>=) = cmp (>=)

cmp :: (BS.ByteString -> BS.ByteString -> Bool) -> (Text -> Text -> Bool)
cmp op (T x) (T y) = op x y

instance Show Text where
  showsPrec p = showsPrec p . unpack

instance IsString Text where
  fromString = pack

instance Semigroup Text where
  (<>) = append

instance Monoid Text where
  mempty = empty

empty :: Text
empty = pack []

pack :: String -> Text
pack = primitive "toUTF8"

unpack :: Text -> String
unpack = primitive "fromUTF8"

append :: Text -> Text -> Text
append (T x) (T y) = T (BS.append x y)
