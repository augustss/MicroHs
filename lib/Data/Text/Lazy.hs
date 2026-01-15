-- |
-- Module      : Data.Text.Lazy
-- Copyright   : (c) 2009, 2010, 2012 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- lists of packed arrays.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- The representation used by this module is suitable for high
-- performance use and for streaming large quantities of data.  It
-- provides a means to manipulate a large body of text without
-- requiring that the entire content be resident in memory.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons',
-- have better time complexity than their "Data.Text" equivalents, due
-- to the underlying representation being a list of chunks. For other
-- operations, lazy 'Text's are usually within a few percent of strict
-- ones, but often with better heap usage if used in a streaming
-- fashion. For data larger than available memory, or if you have
-- tight memory constraints, this module will be the only option.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Lazy as L

module Data.Text.Lazy
    (
    -- * Types
      Text
    , LazyText

    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty
    , fromChunks
    , toChunks
    , toStrict
    , fromStrict
    , foldrChunks
    , foldlChunks

    -- * Pattern matching
    , pattern Empty
    , pattern (:<)
    , pattern (:>)

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , length
    , compareLength

    , replicate
    , splitOn
    , dropWhileEnd
    , map
    , concat
    ) where
import qualified Prelude(); import MiniPrelude hiding(head)
import Primitives
import Control.DeepSeq.Class
import Data.Bounded
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Data
import Data.Int.Int64
import qualified Data.List as L
import Data.String
import qualified Data.Text as T
import Data.Text.Internal
import Text.Read.Internal

data Text = Empty | Chunk !T.Text Text

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
  mappend = (<>)
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
pack s = let (c, s') = L.splitAt defaultChunkSize s in Chunk (T.pack c) (pack s')

unpack :: Text -> String
unpack = foldrChunks (\t s -> T.unpack t ++ s) ""

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
        loop r  [] = [reverse r]
        loop r  s@(c:cs) | Just t <- L.stripPrefix sep s = reverse r : loop [] t
                        | otherwise = loop (c:r) cs

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = pack . L.dropWhileEnd p . unpack

map :: (Char -> Char) -> Text -> Text
map f = foldrChunks (Chunk . T.map f) Empty

concat :: [Text] -> Text
concat []                    = Empty
concat (Empty : css)         = concat css
concat (Chunk c Empty : css) = Chunk c (concat css)
concat (Chunk c cs : css)    = Chunk c (concat (cs : css))


-- | Currently set to 16 KiB, less the memory management overhead.
defaultChunkSize :: Int
defaultChunkSize = 16384 - chunkOverhead

-- | Currently set to 128 bytes, less the memory management overhead.
smallChunkSize :: Int
smallChunkSize = 128 - chunkOverhead

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = _wordSize `primIntShl` 1

emptyError :: String -> a
emptyError fun = error ("Data.Text.Lazy." ++ fun ++ ": empty input")

intToInt64 :: Int -> Int64
intToInt64 = primIntToInt64

int64ToInt :: Int64 -> Int
int64ToInt = primInt64ToInt
