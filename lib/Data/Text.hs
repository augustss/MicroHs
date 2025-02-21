module Data.Text(
  Text,
  pack, unpack,
  empty,
  singleton,
  append,
  null,
  head,
  tail,
  uncons,
  replicate,
  splitOn,
  dropWhileEnd,
  ) where
import qualified Prelude(); import MiniPrelude hiding(head, tail, null, length)
import Control.DeepSeq.Class
import qualified Data.List as L
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

instance NFData Text where
  rnf (T bs) = seq bs ()

empty :: Text
empty = pack []

singleton :: Char -> Text
singleton c = pack [c]

pack :: String -> Text
pack s = T (_primitive "toUTF8" s)

unpack :: Text -> String
unpack (T t) = _primitive "fromUTF8" t

append :: Text -> Text -> Text
append (T x) (T y) = T (BS.append x y)

null :: Text -> Bool
null (T bs) = BS.null bs

length :: Text -> Int
length = L.length . unpack

head :: Text -> Char
head (T t) = _primitive "headUTF8" t

tail :: Text -> Text
tail (T t) = _primitive "tailUTF8" t

uncons :: Text -> Maybe (Char, Text)
uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)

replicate :: Int -> Text -> Text
replicate = stimes

splitOn :: Text -> Text -> [Text]
splitOn s t = map pack $ splitOnList (unpack s) (unpack t)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = pack . L.dropWhileEnd p . unpack

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList [] = error "splitOn: empty"
splitOnList sep = loop []
  where
    loop r  [] = [reverse r]
    loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = reverse r : loop [] t
                     | otherwise = loop (c:r) cs
