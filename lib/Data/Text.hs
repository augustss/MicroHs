module Data.Text(
  Text,
  pack, unpack,
  empty,
  append,
  head,
  ) where
import Prelude(); import MiniPrelude hiding(head)
import Data.Monoid.Internal
import Data.String
import qualified Data.ByteString.Internal as BS

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
pack s = T (primitive "toUTF8" s)

unpack :: Text -> String
unpack (T t) = primitive "fromUTF8" t

append :: Text -> Text -> Text
append (T x) (T y) = T (BS.append x y)

head :: Text -> Char
head (T t) = primitive "headUTF8" t
