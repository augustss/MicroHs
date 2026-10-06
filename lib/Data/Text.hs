module Data.Text(
  Text,
  StrictText,

  pattern Empty,
  pattern (:<),
  pattern (:>),
  pack,
  unpack,
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
  unwords,
  toLower,
  toTitle,
  toUpper,
  toCaseFold,
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
  replace,
  map,
  dropAround,
  strip,
  stripStart,
  stripEnd,
  stripPrefix,
  stripSuffix,
  all,
  any,
  concatMap,
  foldl,
  foldl',
  filter,
  reverse,
  last,
  init,
  elem,
  zip,
  span,
  break,
  breakOn,
  takeWhileEnd,
  count,
  index,
  chunksOf,
  breakOnEnd,
  center,
  compareLength,
  dropEnd,
  find,
  findIndex,
  group,
  groupBy,
  inits,
  tails,
  intersperse,
  justifyLeft,
  justifyRight,
  mapAccumL,
  mapAccumR,
  maximum,
  minimum,
  partition,
  scanl,
  scanr,
  split,
  splitAt,
  takeEnd,
  transpose,
  unfoldr,
  unfoldrN,
  unsnoc,
  zipWith,
  foldl1,
  foldr1,
  scanl1,
  scanr1,
  ) where
import qualified Prelude(); import MiniPrelude hiding(head, tail, null, length, words, unwords, map,
  concatMap, foldl, any, all, filter, reverse, last, init, elem, zip, span, break,
  maximum, minimum, scanl, scanr, splitAt, zipWith, foldl1, foldr1, scanl1, scanr1)
import Control.DeepSeq.Class
import qualified Data.Char as C
import qualified Data.Char.Unicode as U
import qualified Data.List as L
import Data.String
import qualified Data.ByteString.Internal as BS
import Data.Text.Internal
import Text.Read.Internal

type StrictText = Text

pattern Empty :: Text
pattern Empty <- (null -> True) where
  Empty = empty

infixr 5 :<
pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons

infixl 5 :>
pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc

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

instance Read Text where
  readsPrec p str = [(pack x, y) | (x, y) <- readsPrec p str]

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
pack = T . BS.packUTF8

unpack :: Text -> String
unpack (T t) = BS.primBSfromUTF8 t

append :: Text -> Text -> Text
append (T x) (T y) = T (BS.append x y)

null :: Text -> Bool
null (T bs) = BS.null bs

length :: Text -> Int
length = L.length . unpack

head :: Text -> Char
head (T t)
  | BS.null t = error "Data.Text.head: empty"
  | otherwise = BS.primBSheadUTF8 t

cons :: Char -> Text -> Text
cons c t = singleton c `append` t

snoc :: Text -> Char -> Text
snoc t c = t `append` singleton c

tail :: Text -> Text
tail (T t)
  | BS.null t = error "Data.Text.tail: empty"
  | otherwise = T (BS.primBStailUTF8 t)

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
    loop r  [] = [L.reverse r]
    loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = L.reverse r : loop [] t
                     | otherwise = loop (c:r) cs

words :: Text -> [Text]
words = L.map pack . L.words . unpack

unwords :: [Text] -> Text
unwords = pack . L.unwords . L.map unpack

toLower :: Text -> Text
toLower = pack . L.concatMap U._toLowers . unpack

toTitle :: Text -> Text
toTitle = pack . f . unpack
  where f [] = []
        f (c:cs) | U.isAlpha c = U._toTitles c ++ g cs
                 | otherwise   = c : f cs
        g [] = []
        g (c:cs) | U.isAlpha c = U._toLowers c ++ g cs
                 | otherwise   = c : f cs

toUpper :: Text -> Text
toUpper = pack . L.concatMap U._toUppers . unpack

toCaseFold :: Text -> Text
toCaseFold = pack . L.concatMap U._toFolds . unpack

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

replace :: Text -> Text -> Text -> Text
replace s r = intercalate r . splitOn s

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

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

stripStart :: Text -> Text
stripStart = dropWhile C.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd C.isSpace

strip :: Text -> Text
strip = dropAround C.isSpace

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t = pack <$> L.stripPrefix (unpack p) (unpack t)

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t = pack <$> L.stripSuffix (unpack p) (unpack t)

all :: (Char -> Bool) -> Text -> Bool
all p = L.all p . unpack

any :: (Char -> Bool) -> Text -> Bool
any p = L.any p . unpack

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = L.foldr (append . f) empty . unpack

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = L.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

filter :: (Char -> Bool) -> Text -> Text
filter p = pack . L.filter p . unpack

reverse :: Text -> Text
reverse = pack . L.reverse . unpack

last :: Text -> Char
last = L.last . unpack

init :: Text -> Text
init = pack . L.init . unpack

elem :: Char -> Text -> Bool
elem c = L.elem c . unpack

zip :: Text -> Text -> [(Char, Char)]
zip a b = L.zip (unpack a) (unpack b)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = case L.span p (unpack t) of (a, b) -> (pack a, pack b)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)

-- | Split at the first occurrence of the pattern (which is part of the second component).
breakOn :: Text -> Text -> (Text, Text)
breakOn p t = go [] (unpack t)
  where ps = unpack p
        go acc s@(c:cs) | ps `L.isPrefixOf` s = (pack (L.reverse acc), pack s)
                        | otherwise = go (c:acc) cs
        go acc [] = (pack (L.reverse acc), empty)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = pack . L.reverse . L.takeWhile p . L.reverse . unpack

-- | Number of non-overlapping occurrences of the pattern.
count :: Text -> Text -> Int
count p t = L.length (splitOn p t) - 1

index :: Text -> Int -> Char
index t i = unpack t L.!! i

chunksOf :: Int -> Text -> [Text]
chunksOf n t | n <= 0 || null t = []
             | otherwise = take n t : chunksOf n (drop n t)

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd p t = case breakOn (reverse p) (reverse t) of (a, b) -> (reverse b, reverse a)

replicateChar :: Int -> Char -> Text
replicateChar n c = pack (L.replicate n c)

center :: Int -> Char -> Text -> Text
center k c t | len >= k  = t
             | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d = k - len
        r = d `quot` 2
        l = d - r

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft k c t | len >= k  = t
                  | otherwise = t `append` replicateChar (k - len) c
  where len = length t

justifyRight :: Int -> Char -> Text -> Text
justifyRight k c t | len >= k  = t
                   | otherwise = replicateChar (k - len) c `append` t
  where len = length t

compareLength :: Text -> Int -> Ordering
compareLength t n = compare (length t) n

dropEnd :: Int -> Text -> Text
dropEnd n = pack . L.reverse . L.drop n . L.reverse . unpack

takeEnd :: Int -> Text -> Text
takeEnd n = pack . L.reverse . L.take n . L.reverse . unpack

find :: (Char -> Bool) -> Text -> Maybe Char
find p = L.find p . unpack

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p = L.findIndex p . unpack

group :: Text -> [Text]
group = L.map pack . L.group . unpack

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy f = L.map pack . L.groupBy f . unpack

inits :: Text -> [Text]
inits = L.map pack . L.inits . unpack

tails :: Text -> [Text]
tails = L.map pack . L.tails . unpack

intersperse :: Char -> Text -> Text
intersperse c = pack . L.intersperse c . unpack

mapAccumL :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL f z t = case L.mapAccumL f z (unpack t) of (a, s) -> (a, pack s)

mapAccumR :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR f z t = case L.mapAccumR f z (unpack t) of (a, s) -> (a, pack s)

maximum :: Text -> Char
maximum = L.maximum . unpack

minimum :: Text -> Char
minimum = L.minimum . unpack

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = case L.partition p (unpack t) of (a, b) -> (pack a, pack b)

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z = pack . L.scanl f z . unpack

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f z = pack . L.scanr f z . unpack

-- | Split on characters satisfying the predicate.
split :: (Char -> Bool) -> Text -> [Text]
split p t = L.map pack (go (unpack t))
  where go s = case L.break p s of
                 (a, [])       -> [a]
                 (a, _ : rest) -> a : go rest

splitAt :: Int -> Text -> (Text, Text)
splitAt n t = case L.splitAt n (unpack t) of (a, b) -> (pack a, pack b)

transpose :: [Text] -> [Text]
transpose = L.map pack . L.transpose . L.map unpack

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr f = pack . L.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN n f = pack . L.take n . L.unfoldr f

unsnoc :: Text -> Maybe (Text, Char)
unsnoc t = case unpack t of
             [] -> Nothing
             s  -> Just (pack (L.init s), L.last s)

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f a b = pack (L.zipWith f (unpack a) (unpack b))

foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f = L.foldl1 f . unpack

foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f = L.foldr1 f . unpack

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f = pack . L.scanl1 f . unpack

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f = pack . L.scanr1 f . unpack
