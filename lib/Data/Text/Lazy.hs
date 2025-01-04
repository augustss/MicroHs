-- XXX Use coerce of Data.Text
module Data.Text.Lazy(
  Text,
  pack, unpack,
  empty,
  append,
  null,
  head,
  tail,
  uncons,
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

null :: Text -> Bool
null (T bs) = BS.null bs

head :: Text -> Char
head (T t) = primitive "headUTF8" t

tail :: Text -> Text
tail (T t) = primitive "tailUTF8" t

uncons :: Text -> Maybe (Char, Text)
uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)
