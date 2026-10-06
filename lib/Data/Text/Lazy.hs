module Data.Text.Lazy(
  -- Special for Data.Text.Lazy
  Text, LazyText,
  fromChunks,
  toChunks,
  toStrict,
  fromStrict,
  foldrChunks,
  foldlChunks,

  -- Common with Data.Text
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
import qualified Prelude(); import MiniPrelude hiding(head)
import Primitives
import Control.DeepSeq.Class
import Data.Bounded
import qualified Data.Char as C
import qualified Data.Char.Unicode as U
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Data
import Data.Int.Int64
import qualified Data.List as L
import Data.String
import qualified Data.Text as T
import Data.Text.Internal
import Text.Read.Internal

data Text = Empty | Chunk !T.Text Text  -- invariant: T.Text is never empty

type LazyText = Text

instance Eq Text where
  (==) = equal

equal :: Text -> Text -> Bool
equal Empty Empty = True
equal Empty _     = False
equal _     Empty = False
equal (Chunk (T a) as) (Chunk (T b) bs) =
  let
    lenA = BS.length a
    lenB = BS.length b
  in case compare lenA lenB of
    LT -> a == BS.unsafeTake lenA b
          && as `equal` Chunk (T (BS.unsafeDrop lenA b)) bs
    EQ -> a == b
          && as `equal` bs
    GT -> BS.unsafeTake lenB a == b
          && Chunk (T (BS.unsafeDrop lenB a)) as `equal` bs

instance Ord Text where
  compare = compareText

compareText :: Text -> Text -> Ordering
compareText Empty Empty = EQ
compareText Empty _     = LT
compareText _     Empty = GT
compareText (Chunk (T a) as) (Chunk (T b) bs) =
  let
    lenA = BS.length a
    lenB = BS.length b
  in case compare lenA lenB of
    LT -> case compare a (BS.unsafeTake lenA b) of
            EQ     -> compareText as (Chunk (T (BS.unsafeDrop lenA b)) bs)
            result -> result
    EQ -> case compare a b of
            EQ     -> compareText as bs
            result -> result
    GT -> case compare (BS.unsafeTake lenB a) b of
            EQ     -> compareText (Chunk (T (BS.unsafeDrop lenB a)) as) bs
            result -> result
-- This is not a mistake: on contrary to UTF-16 (https://github.com/haskell/text/pull/208),
-- lexicographic ordering of UTF-8 encoded strings matches lexicographic ordering
-- of underlying bytearrays, no decoding is needed.

instance Show Text where
  showsPrec p t = showsPrec p (unpack t)

instance Read Text where
  readsPrec p str = [(pack x, y) | (x, y) <- readsPrec p str]

instance Semigroup Text where
  (<>) = append
  stimes n _ | n < 0 = error "Data.Text.Lazy.stimes: given number is negative!"
  stimes n a =
    let nInt64 = fromIntegral n :: Int64
        len = if n == fromIntegral nInt64 && nInt64 >= 0 then nInt64 else maxBound
        -- We clamp the length to maxBound :: Int64.
        -- To tell the difference, the caller would have to skip through 2^63 chunks.
    in replicate len a

instance Monoid Text where
  mempty  = empty
  mconcat = concat

instance IsString Text where
  fromString = pack

instance NFData Text where
  rnf Empty        = ()
  rnf (Chunk _ ts) = rnf ts

{-
instance Binary Text where
  put t = do
    -- This needs to be in sync with the Binary instance for ByteString
    -- in the binary package.
    put (foldlChunks (\n c -> n + T.lengthWord8 c) 0 t)
    putBuilder (encodeUtf8Builder t)
  get   = do
    bs <- get
    case decodeUtf8' bs of
      Left exn -> fail (show exn)
      Right a -> return a
-}

instance Data Text where
  gfoldl f z txt = z pack `f` unpack txt
  toConstr _     = packConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z pack)
    _ -> error "Data.Text.Lazy.Text.gunfold"
  dataTypeOf _   = textDataType

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Lazy.Text" [packConstr]

pack :: String -> Text
pack "" = Empty
pack s =
  case L.splitAt defaultChunkSize s of
    (c, s') -> Chunk (T.pack c) (pack s')

unpack :: Text -> String
unpack = foldrChunks (\ t s -> T.unpack t ++ s) ""

singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty

chunk :: T.Text -> Text -> Text
chunk t ts | T.null t = ts
           | otherwise = Chunk t ts

empty :: Text
empty = Empty

fromChunks :: [T.Text] -> Text
fromChunks = L.foldr chunk Empty

toChunks :: Text -> [T.Text]
toChunks = foldrChunks (:) []

onChunks :: (T.Text -> T.Text) -> Text -> Text
onChunks f = foldrChunks (Chunk . f) Empty

toStrict :: Text -> T.Text
toStrict t = T.concat (toChunks t)

fromStrict :: T.Text -> Text
fromStrict t = chunk t Empty

foldrChunks :: (T.Text -> a -> a) -> a -> Text -> a
foldrChunks f z = go
  where go Empty        = z
        go (Chunk c cs) = f c (go cs)

foldlChunks :: (a -> T.Text -> a) -> a -> Text -> a
foldlChunks f z = go z
  where go !a Empty        = a
        go !a (Chunk c cs) = go (f a c) cs

pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons
infixr 5 :<

pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
infixl 5 :>

-- -----------------------------------------------------------------------------
-- * Basic functions

cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t

infixr 5 `cons`

snoc :: Text -> Char -> Text
snoc t c = foldrChunks Chunk (singleton c) t

append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs

uncons :: Text -> Maybe (Char, Text)
uncons Empty        = Nothing
uncons (Chunk t ts) = Just (T.head t, chunk (T.tail t) ts)

head :: Text -> Char
head (Chunk t _) = T.head t
head Empty       = emptyError "head"

tail :: Text -> Text
tail (Chunk t ts) = chunk (T.tail t) ts
tail Empty        = emptyError "tail"

unsnoc :: Text -> Maybe (Text, Char)
unsnoc Empty          = Nothing
unsnoc ts@(Chunk _ _) = Just (init ts, last ts)

last :: Text -> Char
last = L.last . unpack
{-
last (Chunk t ts) = go t ts
    where go _ (Chunk t' ts') = go t' ts'
          go t' Empty         = T.last t'
last Empty = emptyError "last"
-}

init :: Text -> Text
init = pack . L.init . unpack
{-
init (Chunk t0 ts0) = go t0 ts0
    where go t (Chunk t' ts) = Chunk t (go t' ts)
          go t Empty         = chunk (T.init t) Empty
init Empty = emptyError "init"
-}

null :: Text -> Bool
null Empty = True
null _     = False

-- | /O(n)/ Returns the number of characters in a 'Text'.
length :: Text -> Int64
length = foldlChunks go 0
    where
        go :: Int64 -> T.Text -> Int64
        go l t = l + intToInt64 (T.length t)

compareLength :: Text -> Int64 -> Ordering
compareLength t = compareLengthList (unpack t)
  where
    compareLengthList xs n
      | n < 0 = GT
      | otherwise = foldr
        (\_ f m -> if m > 0 then f (m - 1) else GT)
        (\m -> if m > 0 then LT else EQ)
        xs
        n

replicate :: Int64 -> Text -> Text
replicate n
  | n <= 0 = const Empty
  | otherwise = \case
    Empty -> Empty
    t -> concat (L.genericReplicate n t)

splitOn :: Text -> Text -> [Text]
splitOn pat src = L.map pack $ splitOnList (unpack pat) (unpack src)
  where
    splitOnList :: Eq a => [a] -> [a] -> [[a]]
    splitOnList [] = error "splitOn: empty"
    splitOnList sep = loop []
      where
        loop r  [] = [L.reverse r]
        loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = L.reverse r : loop [] t
                         | otherwise = loop (c:r) cs

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = pack . L.dropWhileEnd p . unpack

map :: (Char -> Char) -> Text -> Text
map f = onChunks (T.map f)

concat :: [Text] -> Text
concat []                    = Empty
concat (Empty : css)         = concat css
concat (Chunk c Empty : css) = Chunk c (concat css)
concat (Chunk c cs : css)    = Chunk c (concat (cs : css))


-- | Currently set to 16 KiB, less the memory management overhead.
defaultChunkSize :: Int
defaultChunkSize = 16384 - 32

emptyError :: String -> a
emptyError fun = error ("Data.Text.Lazy." ++ fun ++ ": empty input")

intToInt64 :: Int -> Int64
intToInt64 = primIntToInt64

int64ToInt :: Int64 -> Int
int64ToInt = primInt64ToInt

-----
-- XXX This is pretty much exact copies of Data.Text.
-- That's crazy!  Strict text should be implemented as a single big chunk
-- of lazy text.

take :: Int64 -> Text -> Text
take n = pack . L.take (int64ToInt n) . unpack

drop :: Int64 -> Text -> Text
drop n = pack . L.drop (int64ToInt n) . unpack

toLower :: Text -> Text
toLower = onChunks T.toLower

toTitle :: Text -> Text
toTitle = onChunks T.toTitle

toUpper :: Text -> Text
toUpper = onChunks T.toUpper

toCaseFold :: Text -> Text
toCaseFold = onChunks T.toCaseFold

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

replace :: Text -> Text -> Text -> Text
replace s r = intercalate r . splitOn s

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = pack . L.dropWhile p . unpack

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = pack . L.takeWhile p . unpack

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

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = L.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

filter :: (Char -> Bool) -> Text -> Text
filter p = onChunks (T.filter p)

reverse :: Text -> Text
reverse = pack . L.reverse . unpack

breakOn :: Text -> Text -> (Text, Text)
breakOn p t = go [] (unpack t)
  where ps = unpack p
        go acc s@(c:cs) | ps `L.isPrefixOf` s = (pack (L.reverse acc), pack s)
                        | otherwise = go (c:acc) cs
        go acc [] = (pack (L.reverse acc), empty)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = pack . L.reverse . L.takeWhile p . L.reverse . unpack

count :: Text -> Text -> Int
count p t = L.length (splitOn p t) - 1

index :: Text -> Int64 -> Char
index t i = unpack t L.!! int64ToInt i

chunksOf :: Int64 -> Text -> [Text]
chunksOf n t | n <= 0 || null t = []
             | otherwise = take n t : chunksOf n (drop n t)

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd p t = case breakOn (reverse p) (reverse t) of (a, b) -> (reverse b, reverse a)

replicateChar :: Int64 -> Char -> Text
replicateChar n c = pack (L.replicate (int64ToInt n) c)

center :: Int64 -> Char -> Text -> Text
center k c t | len >= k  = t
             | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d = k - len
        r = d `quot` 2
        l = d - r

justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft k c t | len >= k  = t
                  | otherwise = t `append` replicateChar (k - len) c
  where len = length t

justifyRight :: Int64 -> Char -> Text -> Text
justifyRight k c t | len >= k  = t
                   | otherwise = replicateChar (k - len) c `append` t
  where len = length t

dropEnd :: Int64 -> Text -> Text
dropEnd n = pack . L.reverse . L.drop (int64ToInt n) . L.reverse . unpack

takeEnd :: Int64 -> Text -> Text
takeEnd n = pack . L.reverse . L.take (int64ToInt n) . L.reverse . unpack

find :: (Char -> Bool) -> Text -> Maybe Char
find p = L.find p . unpack

findIndex :: (Char -> Bool) -> Text -> Maybe Int64
findIndex p = fmap intToInt64 . L.findIndex p . unpack

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

split :: (Char -> Bool) -> Text -> [Text]
split p t = L.map pack (go (unpack t))
  where go s = case L.break p s of
                 (a, [])       -> [a]
                 (a, _ : rest) -> a : go rest

splitAt :: Int64 -> Text -> (Text, Text)
splitAt n t = case L.splitAt (int64ToInt n) (unpack t) of (a, b) -> (pack a, pack b)

transpose :: [Text] -> [Text]
transpose = L.map pack . L.transpose . L.map unpack

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr f = pack . L.unfoldr f

unfoldrN :: Int64 -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN n f = pack . L.take (int64ToInt n) . L.unfoldr f
