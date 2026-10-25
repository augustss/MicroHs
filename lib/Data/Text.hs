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
import Data.List.NonEmpty_Type(NonEmpty(..))
import Data.String
import qualified Data.ByteString.Internal as BS
import Data.Text.Internal
import Text.Read.Internal
import Primitives(primChr)

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
  sconcat (t :| ts) = concat (t : ts)
  stimes n t
    | n <= 0    = error "stimes: positive multiplier expected"
    | otherwise = replicate (fromIntegral n) t

instance Monoid Text where
  mempty = empty
  mconcat = concat

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
length (T b) = BS.primUTF8length b

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
replicate n t@(T b) | n <= 0    = empty
                    | otherwise = T (BS.concatBS (L.replicate n b))

splitOn :: Text -> Text -> [Text]
splitOn p@(T pb) t@(T tb)
  | null p    = error "splitOn: empty"
  | otherwise = go 0
  where
    n = bytes t
    go i = case BS.primBSfind pb tb i of
             j | j < 0     -> [slice i n t]
               | otherwise -> slice i j t : go (j + bytes p)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(T b) = slice 0 (suffixStart p b) t

words :: Text -> [Text]
words t@(T b) = go 0
  where
    n = bytes t
    go i = case BS.primUTF8skipSpace b i of
             s | s >= n    -> []
               | otherwise -> case BS.primUTF8skipNonSpace b s of
                                e -> slice s e t : go e

unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')

toLower :: Text -> Text
toLower t@(T b)
  | isAsciiText t = T (BS.primBSasciiCase b 0)
  | otherwise     = pack . L.concatMap U._toLowers . unpack $ t

toTitle :: Text -> Text
toTitle = pack . f . unpack
  where f [] = []
        f (c:cs) | U.isAlpha c = U._toTitles c ++ g cs
                 | otherwise   = c : f cs
        g [] = []
        g (c:cs) | U.isAlpha c = U._toLowers c ++ g cs
                 | otherwise   = c : f cs

toUpper :: Text -> Text
toUpper t@(T b)
  | isAsciiText t = T (BS.primBSasciiCase b 1)
  | otherwise     = pack . L.concatMap U._toUppers . unpack $ t

toCaseFold :: Text -> Text
toCaseFold t@(T b)
  | isAsciiText t = T (BS.primBSasciiCase b 0)
  | otherwise     = pack . L.concatMap U._toFolds . unpack $ t

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z = L.foldr f z . unpack

concat :: [Text] -> Text
concat []  = empty
concat [t] = t
concat ts  = T (BS.concatBS (L.map (\ (T b) -> b) ts))

unlines :: [Text] -> Text
unlines ls = concat (L.concatMap (\ l -> [l, newline]) ls)

lines :: Text -> [Text]
lines t@(T b) = go 0
  where
    n = bytes t
    go i | i >= n    = []
         | otherwise = case BS.primBSfind newlineBS b i of
                         j | j < 0     -> [slice i n t]
                           | otherwise -> slice i j t : go (j + 1)

take :: Int -> Text -> Text
take n t@(T b) = slice 0 (BS.primUTF8offset b n) t

drop :: Int -> Text -> Text
drop n t@(T b) = slice (BS.primUTF8offset b n) (bytes t) t

intercalate :: Text -> [Text] -> Text
intercalate s ts = concat (L.intersperse s ts)

replace :: Text -> Text -> Text -> Text
replace s r
  | null s    = error "Data.Text.replace: empty input"
  | otherwise = intercalate r . splitOn s

isPrefixOf :: Text -> Text -> Bool
isPrefixOf p t = bytes p <= bytes t && slice 0 (bytes p) t == p

isSuffixOf :: Text -> Text -> Bool
isSuffixOf p t = bytes p <= bytes t && slice (bytes t - bytes p) (bytes t) t == p

isInfixOf :: Text -> Text -> Bool
isInfixOf (T p) (T t) = BS.primBSfind p t 0 >= 0

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t@(T b) = slice (prefixEnd p b 0) (bytes t) t

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t@(T b) = slice 0 (prefixEnd p b 0) t

map :: (Char -> Char) -> Text -> Text
map f = pack . L.map f . unpack

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

stripStart :: Text -> Text
stripStart t@(T b) = slice (BS.primUTF8skipSpace b 0) (bytes t) t

stripEnd :: Text -> Text
stripEnd t@(T b) = slice 0 (BS.primUTF8trimSpaceEnd b) t

strip :: Text -> Text
strip t@(T b) = slice (BS.primUTF8skipSpace b 0) (BS.primUTF8trimSpaceEnd b) t

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t
  | p `isPrefixOf` t = Just (slice (bytes p) (bytes t) t)
  | otherwise      = Nothing

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t
  | p `isSuffixOf` t = Just (slice 0 (bytes t - bytes p) t)
  | otherwise      = Nothing

all :: (Char -> Bool) -> Text -> Bool
all p (T b) = L.all (L.all p) (chunksFrom b)

any :: (Char -> Bool) -> Text -> Bool
any p (T b) = L.any (L.any p) (chunksFrom b)

concatMap :: (Char -> Text) -> Text -> Text
concatMap f t = concat [ f c | c <- unpack t ]

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = L.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

filter :: (Char -> Bool) -> Text -> Text
filter p = pack . L.filter p . unpack

reverse :: Text -> Text
reverse (T b) = T (BS.primUTF8reverse b)

last :: Text -> Char
last t@(T b)
  | null t    = error "Data.Text.last: empty"
  | otherwise = charOf (BS.primUTF8decodeEnd b (bytes t))

init :: Text -> Text
init t@(T b)
  | null t    = error "Data.Text.init: empty"
  | otherwise = slice 0 (BS.primUTF8offsetEnd b 1) t

elem :: Char -> Text -> Bool
elem c t
  | c >= '\xD800' && c <= '\xDFFF' = False   -- never in a Text ('pack' replaces them)
  | otherwise = singleton c `isInfixOf` t

zip :: Text -> Text -> [(Char, Char)]
zip a b = L.zip (unpack a) (unpack b)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p t@(T b) = case prefixEnd p b 0 of i -> (slice 0 i t, slice i (bytes t) t)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)

-- | Split at the first occurrence of the pattern (which is part of the second component).
breakOn :: Text -> Text -> (Text, Text)
breakOn p'@(T p) t@(T b)
  | null p'   = error "Data.Text.breakOn: empty input"
  | otherwise =
      case BS.primBSfind p b 0 of
        i | i < 0     -> (t, empty)
          | otherwise -> (slice 0 i t, slice i (bytes t) t)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p t@(T b) = slice (suffixStart p b) (bytes t) t

-- | Number of non-overlapping occurrences of the pattern.
count :: Text -> Text -> Int
count p@(T pb) (T b)
  | null p    = error "Data.Text.count: empty pattern"
  | otherwise = go 0 0
  where
    go k i = case BS.primBSfind pb b i of
               j | j < 0     -> k
                 | otherwise -> go (k + 1) (j + bytes p)

index :: Text -> Int -> Char
index t@(T b) i
  | i < 0     = error "Data.Text.index: negative index"
  | o >= bytes t = error "Data.Text.index: index too large"
  | otherwise = charOf (BS.primUTF8decode b o)
  where o = BS.primUTF8offset b i

chunksOf :: Int -> Text -> [Text]
chunksOf n t | n <= 0 || null t = []
             | otherwise = take n t : chunksOf n (drop n t)

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd p@(T pb) t@(T b)
  | null p    = error "Data.Text.breakOnEnd: empty input"
  | otherwise =
      case BS.primBSrfind pb b of
        i | i < 0     -> (empty, t)
          | otherwise -> case i + bytes p of j -> (slice 0 j t, slice j (bytes t) t)

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
compareLength t@(T b) n
  | n < 0 = GT
  -- more than n characters: stop after the first n
  | BS.primUTF8offset b n < bytes t = GT
  | otherwise = compare (length t) n

dropEnd :: Int -> Text -> Text
dropEnd n t@(T b) = slice 0 (BS.primUTF8offsetEnd b n) t

takeEnd :: Int -> Text -> Text
takeEnd n t@(T b) = slice (BS.primUTF8offsetEnd b n) (bytes t) t

find :: (Char -> Bool) -> Text -> Maybe Char
find p (T b) = go (chunksFrom b)
  where
    go []     = Nothing
    go (c:cs) = case L.find p c of
                  Nothing -> go cs
                  r       -> r

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p (T b) = go 0 (chunkSizes `L.zip` chunksFrom b)
  where
    -- every chunk before the last one has exactly its size in characters
    go k []            = k `seq` Nothing
    go k ((m, c) : cs) = k `seq` case L.findIndex p c of
                                   Just j  -> Just (k + j)
                                   Nothing -> go (k + m) cs

group :: Text -> [Text]
group = groupBy (==)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy f = L.map pack . L.groupBy f . unpack

inits :: Text -> [Text]
inits t = L.map (\ i -> slice 0 i t) (boundaries t)

tails :: Text -> [Text]
tails t = L.map (\ i -> slice i (bytes t) t) (boundaries t)

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
splitAt n t@(T b) = case BS.primUTF8offset b n of i -> (slice 0 i t, slice i (bytes t) t)

transpose :: [Text] -> [Text]
transpose = L.map pack . L.transpose . L.map unpack

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr f = pack . L.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN n f = pack . L.take n . L.unfoldr f

unsnoc :: Text -> Maybe (Text, Char)
unsnoc t | null t    = Nothing
         | otherwise = Just (init t, last t)

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

-----------------------------------------------------------------------------
-- Byte-level helpers.  A Text is a (modified) UTF-8 ByteString; the
-- primitives in Data.ByteString.Internal work on byte offsets, so most
-- functions slice the original bytes instead of going through a String.

bytes :: Text -> Int
bytes (T b) = BS.primBSlength b

-- | The bytes from offset @i@ up to (not including) offset @j@.
slice :: Int -> Int -> Text -> Text
slice i j t@(T b)
  | i <= 0 && j >= BS.primBSlength b = t
  | j <= i    = empty
  | otherwise = T (BS.primBSsubstr b i (j - i))

-- | The character and byte count in a result of 'BS.primUTF8decode'.
charOf :: Int -> Char
charOf r = primChr (r `quot` 8)

widthOf :: Int -> Int
widthOf r = r `rem` 8

-- The predicate is Haskell, so these look at every character in turn.
-- Decoding one character per primitive call costs about three times as
-- much as walking a String that the runtime has unpacked, so the bytes
-- are unpacked in chunks: 16 characters first, doubling up to 'maxChunk',
-- which keeps an early exit cheap and a long walk linear.  The last
-- 'smallRest' bytes are unpacked at once.

maxChunk :: Int
maxChunk = 4096

smallRest :: Int
smallRest = 256

-- | The next chunk of @b@ from byte offset @i@: at most @sz@ characters
-- (or all of a small rest).
chunkOf :: Int -> BS.ByteString -> Int -> BS.ByteString
chunkOf sz b i
  | n - i <= smallRest = if i == 0 then b else BS.primBSsubstr b i (n - i)
  | otherwise = BS.primBSsubstr b i (BS.primUTF8offsetFrom b i sz - i)
  where n = BS.primBSlength b

-- | Where the chunk of @b@ that ends at byte offset @i@ starts: at most
-- @sz@ characters (or all of a small front).
chunkStartBefore :: Int -> BS.ByteString -> Int -> Int
chunkStartBefore sz b i
  | i <= smallRest = 0
  | otherwise = BS.primUTF8offsetEndBefore b i sz

-- | Offset of the first character at or after @i@ that does not satisfy @p@.
prefixEnd :: (Char -> Bool) -> BS.ByteString -> Int -> Int
prefixEnd p b = go 16
  where
    n = BS.primBSlength b
    go sz i
      | i >= n    = n
      | otherwise =
          case chunkOf sz b i of
            chunk -> case leading p (BS.primBSfromUTF8 chunk) of
              k | k < 0     -> go (min maxChunk (sz * 2)) (i + BS.primBSlength chunk)
                | otherwise -> i + BS.primUTF8offset chunk k

-- | Offset where the longest suffix whose characters satisfy @p@ starts.
suffixStart :: (Char -> Bool) -> BS.ByteString -> Int
suffixStart p b = go 16 (BS.primBSlength b)
  where
    go sz i
      | i <= 0    = 0
      | otherwise =
          case chunkStartBefore sz b i of
            s -> case (if s == 0 && i == BS.primBSlength b then b else BS.primBSsubstr b s (i - s)) of
              chunk -> case leading p (L.reverse (BS.primBSfromUTF8 chunk)) of
                k | k < 0     -> go (min maxChunk (sz * 2)) s
                  | otherwise -> s + BS.primUTF8offsetEnd chunk k

-- | The number of leading characters that satisfy @p@, or -1 if all do.
leading :: (Char -> Bool) -> String -> Int
leading p = go 0
  where
    go k []     = k `seq` (-1)
    go k (c:cs) = k `seq` (if p c then go (k + 1) cs else k)

-- | The characters, unpacked lazily in chunks of 'chunkSizes' characters.
chunksFrom :: BS.ByteString -> [String]
chunksFrom b = go chunkSizes 0
  where
    n = BS.primBSlength b
    go (sz:szs) i
      | i >= n    = []
      | otherwise = case chunkOf sz b i of
                      chunk -> BS.primBSfromUTF8 chunk : go szs (i + BS.primBSlength chunk)
    go [] _ = []

chunkSizes :: [Int]
chunkSizes = L.takeWhile (< maxChunk) (L.iterate (* 2) 16) ++ L.repeat maxChunk

-- | The offset of every character, and the end.
boundaries :: Text -> [Int]
boundaries t@(T b) = go 0
  where
    n = bytes t
    go i | i >= n    = [n]
         | otherwise = i : go (i + widthOf (BS.primUTF8decode b i))

isAsciiText :: Text -> Bool
isAsciiText (T b) = BS.primBSisAscii b /= 0

newline :: Text
newline = pack "\n"

newlineBS :: BS.ByteString
newlineBS = case newline of T b -> b
