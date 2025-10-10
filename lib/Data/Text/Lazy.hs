module Data.Text.Lazy(
  Text,
  toStrict, toLazy,
  pack, unpack,
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
  map,
  concat,
  ) where
import qualified Prelude(); import MiniPrelude hiding(head)
import Control.DeepSeq.Class
import Data.Coerce
import Data.Monoid
import Data.String
import qualified Data.Text as T

newtype Text = L T.Text
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid, NFData)

toStrict :: Text -> T.Text
toStrict (L t) = t

toLazy :: T.Text -> Text
toLazy = L

empty :: Text
empty = coerce T.empty

singleton :: Char -> Text
singleton = coerce T.singleton

pack :: String -> Text
pack = coerce T.pack

unpack :: Text -> String
unpack = coerce T.unpack

append :: Text -> Text -> Text
append = coerce T.append

null :: Text -> Bool
null = coerce T.null

length :: Text -> Int
length = coerce T.length

head :: Text -> Char
head = coerce T.head

tail :: Text -> Text
tail = coerce T.tail

cons :: Char -> Text -> Text
cons = coerce T.cons

snoc :: Text -> Char -> Text
snoc = coerce T.snoc

uncons :: Text -> Maybe (Char, Text)
uncons = coerce T.uncons

replicate :: Int -> Text -> Text
replicate n = coerce (T.replicate n)

splitOn :: Text -> Text -> [Text]
splitOn = coerce T.splitOn

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd = coerce T.dropWhileEnd

map :: (Char -> Char) -> Text -> Text
map f = coerce (T.map f)

concat :: [Text] -> Text
concat = coerce T.concat
