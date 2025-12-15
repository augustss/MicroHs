module Data.Text(
  Text,
  pack, unpack,
  show,
  empty,
  singleton,
  append,
  null,
  length,
  head,
  tail,
  cons,
  snoc,
  uncons,
  replicate,
  splitOn,
  dropWhileEnd,
  words,
  foldr,
  concat,
  lines,
  unlines,
  take,
  drop,
  takeWhile,
  dropWhile,
  dropWhileEnd,
  intercalate,
  isPrefixOf,
  isSuffixOf,
  isInfixOf,
  map,
  ) where
import qualified Prelude(); import MiniPrelude hiding(head, tail, null, length, words, map)
import Primitives(Ptr)
import Control.DeepSeq.Class
import qualified Data.List as L
import Data.Monoid.Internal
import Data.String
import qualified Data.ByteString.Internal as BS
import Foreign.C.Types(CChar)
import Unsafe.Coerce(unsafeCoerce)

newtype Text = T BS.ByteString

instance Eq Text where
  (==) = cmp (==)
  (/=) = cmp (/=)

instance Ord Text where
  (<)  = cmp (<)
  (<=) = cmp (<=)
  (>)  = cmp (>)
  (>=) = cmp (>=)

show :: Show a => a -> Text
show = pack . MiniPrelude.show

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
head (T t)
  | BS.null t = error "Data.Text.head: empty"
  | otherwise = _primitive "headUTF8" t

cons :: Char -> Text -> Text
cons c t = singleton c `append` t

snoc :: Text -> Char -> Text
snoc t c = t `append` singleton c

tail :: Text -> Text
tail (T t)
  | BS.null t = error "Data.Text.tail: empty"
  | otherwise = _primitive "tailUTF8" t

uncons :: Text -> Maybe (Char, Text)
uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)

replicate :: Int -> Text -> Text
replicate = stimes

splitOn :: Text -> Text -> [Text]
splitOn s t = L.map pack $ splitOnList (unpack s) (unpack t)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = pack . L.dropWhileEnd p . unpack

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList [] = error "splitOn: empty"
splitOnList sep = loop []
  where
    loop r  [] = [reverse r]
    loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = reverse r : loop [] t
                     | otherwise = loop (c:r) cs

words :: Text -> [Text]
words = L.map pack . L.words . unpack

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z = L.foldr f z . unpack

concat :: [Text] -> Text
concat = L.foldr append empty

unlines :: [Text] -> Text
unlines = L.foldr (\ l -> append (append l (pack "\n"))) empty

lines :: Text -> [Text]
lines = L.map pack . L.lines . unpack

take :: Int -> Text -> Text
take n = pack . L.take n . unpack

drop :: Int -> Text -> Text
drop n = pack . L.drop n . unpack

intercalate :: Text -> [Text] -> Text
intercalate _ [] = empty
intercalate _ [x] = x
intercalate s (x:xs) = x `append` s `append` intercalate s xs

-- XXX Should make the BS version efficient and go via that
isPrefixOf :: Text -> Text -> Bool
isPrefixOf p s = L.isPrefixOf (unpack p) (unpack s)

isSuffixOf :: Text -> Text -> Bool
isSuffixOf p s = L.isSuffixOf (unpack p) (unpack s)

isInfixOf :: Text -> Text -> Bool
isInfixOf p s = L.isInfixOf (unpack p) (unpack s)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = pack . L.dropWhile p . unpack

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = pack . L.takeWhile p . unpack

map :: (Char -> Char) -> Text -> Text
map f = pack . L.map f . unpack
