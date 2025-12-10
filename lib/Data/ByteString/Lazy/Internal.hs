module Data.ByteString.Lazy.Internal where

import Control.DeepSeq.Class
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as C
import Data.Word.Word8
import Data.List.NonEmpty_Type

data ByteString = Empty | Chunk !S.StrictByteString ByteString

type LazyByteString = ByteString

instance Eq ByteString where
  (==) = eq

instance Ord ByteString where
  compare = cmp

instance Semigroup ByteString where
  (<>) = append
  sconcat (b :| bs) = concat (b : bs)
  stimes = times

instance Monoid ByteString where
  mempty = Empty
  mconcat = concat

instance NFData ByteString where
  rnf Empty        = ()
  rnf (Chunk c cs) = rnf c `seq` rnf cs

instance Show ByteString where
  showsPrec p bs = showsPrec p (unpackChars bs)

instance Read ByteString where
  readsPrec p bs = [(packChars x, y) | (x, y) <- readsPrec p bs]

instance IsString ByteString where
  fromString = packChars

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case splitAt n cs of
      (bs, [])  -> chunk (S.pack bs) Empty
      (bs, cs') -> Chunk (S.pack bs) (packChunks (min (n * 2) smallChunkSize) cs')

packChars :: [Char] -> ByteString
packChars cs0 = packChunks 32 cs0
  where
    packChunks n cs = case splitAt n cs of
      (bs, [])  -> chunk (C.pack bs) Empty
      (bs, cs') -> Chunk (C.pack bs) (packChunks (min (n * 2) smallChunkSize) cs')

unpackBytes :: ByteString -> [Word8]
unpackBytes Empty        = []
unpackBytes (Chunk c cs) = S.unpack c ++ unpackBytes cs

unpackChars :: ByteString -> [Char]
unpackChars Empty        = []
unpackChars (Chunk c cs) = C.unpack c ++ unpackChars cs

chunk :: S.StrictByteString -> ByteString -> ByteString
chunk c cs
  | S.null c = cs
  | otherwise = Chunk c cs

foldrChunks :: (S.StrictByteString -> a -> a) -> a -> ByteString -> a
foldrChunks f z = go
  where go Empty        = z
        go (Chunk c cs) = f c (go cs)

foldlChunks :: (a -> S.StrictByteString -> a) -> a -> ByteString -> a
foldlChunks f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (f a c) cs

defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024

chunkOverhead :: Int
chunkOverhead = 2 * _wordSize

eq :: ByteString -> ByteString -> Bool
eq Empty Empty = True
eq Empty _     = False
eq _     Empty = False
eq (Chunk a as) (Chunk b bs) =
  let
    al = S.length a
    bl = S.length b
  in case compare al bl of
    LT -> a == S.unsafeTake al b && eq as (Chunk (S.unsafeDrop al b) bs)
    EQ -> a == b && eq as bs
    GT -> S.unsafeTake bl a == b && eq (Chunk (S.unsafeDrop bl a) as) bs

cmp :: ByteString -> ByteString -> Ordering
cmp Empty Empty = EQ
cmp Empty _     = LT
cmp _     Empty = GT
cmp (Chunk a as) (Chunk b bs) =
  let
    al = S.length a
    bl = S.length b
  in case compare al bl of
    LT -> case compare a (S.unsafeTake al b) of
            EQ     -> cmp as (Chunk (S.unsafeDrop al b) bs)
            result -> result
    EQ -> case compare a b of
            EQ     -> cmp as bs
            result -> result
    GT -> case compare (S.unsafeTake bl a) b of
            EQ     -> cmp (Chunk (S.unsafeDrop bl a) as) bs
            result -> result

append :: ByteString -> ByteString -> ByteString
append xs ys = foldrChunks Chunk ys xs

concat :: [ByteString] -> ByteString
concat = foldr append Empty

times :: Integral a => a -> ByteString -> ByteString
times 0 _ = Empty
times n lbs0
  | n < 0 = error "stimes: non-negative multiplier expected"
  | otherwise = case lbs0 of
    Empty -> Empty
    Chunk bs lbs -> Chunk bs (go lbs)
  where
    go Empty = times (n-1) lbs0
    go (Chunk c cs) = Chunk c (go cs)

fromStrict :: S.StrictByteString -> LazyByteString
fromStrict bs = chunk bs Empty

toStrict :: LazyByteString -> S.StrictByteString
toStrict = foldlChunks S.append S.empty
