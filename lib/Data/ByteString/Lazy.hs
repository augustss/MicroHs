-- |
-- Module      : Data.ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Lazy ByteStrings are encoded as lazy lists of strict chunks
-- of bytes.
--
-- A key feature of lazy ByteStrings is the means to manipulate large or
-- unbounded streams of data without requiring the entire sequence to be
-- resident in memory. To take advantage of this you have to write your
-- functions in a lazy streaming style, e.g. classic pipeline composition. The
-- default I\/O chunk size is 32k, which should be good in most circumstances.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons', have
-- better complexity than their "Data.ByteString" equivalents, due to
-- optimisations resulting from the list spine structure. For other
-- operations lazy ByteStrings are usually within a few percent of
-- strict ones.
--
-- The recomended way to assemble lazy ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'Foreign.ForeignPtr.ForeignPtr'
-- by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
-- Lazy variant by Duncan Coutts and Don Stewart.
--

module Data.ByteString.Lazy (

        -- * Lazy @ByteString@
        ByteString,
        LazyByteString,

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromChunks,
        toChunks,
        foldrChunks,
        foldlChunks,

        -- * Basic interface
        cons,
        cons',
        snoc,
        append,
        head,
        uncons,
        unsnoc,
        last,
        tail,
        init,
        null,
        length,

        -- * Transforming ByteStrings
        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        -- * Reducing 'ByteString's (folds)
        foldl,
        foldl',
        foldl1,
        foldl1',
        foldr,
        foldr',
        foldr1,
        foldr1',

        -- ** Special folds
        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,
        compareLength,

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Infinite ByteStrings
        repeat,
        replicate,
        cycle,
        iterate,

        -- ** Unfolding ByteStrings
        unfoldr,

        -- * Substrings

        -- ** Breaking strings
        take,
        takeEnd,
        drop,
        dropEnd,
        splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        span,
        spanEnd,
        break,
        breakEnd,
        group,
        groupBy,
        inits,
        tails,
        initsNE,
        tailsNE,
        stripPrefix,
        stripSuffix,

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,
--        isInfixOf,

        -- ** Search for arbitrary substrings
--        isSubstringOf,

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,
        notElem,

        -- ** Searching with a predicate
        find,
        filter,
        partition,

        -- * Indexing ByteStrings
        index,
        indexMaybe,
        (!?),
        elemIndex,
        elemIndexEnd,
        elemIndices,
        findIndex,
        findIndexEnd,
        findIndices,
        count,

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
--        sort,

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,
--        defrag,

        -- * I\/O with 'ByteString's
        -- $IOChunk

        -- ** Standard input and output
        getContents,
        putStr,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,

        -- ** I\/O with Handles
        hGetContents,
        hGet,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,

  ) where

import qualified Prelude ()
import MiniPrelude hiding (concat)

import Primitives
import Data.Bifunctor (first)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import Data.ByteString.Lazy.Internal
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Int.Int64 (Int64)
import Data.Word.Word8 (Word8)
import System.IO.Base
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error

empty :: ByteString
empty = Empty

singleton :: Word8 -> ByteString
singleton w = chunk (S.singleton w) Empty

pack :: [Word8] -> ByteString
pack = packBytes

unpack :: ByteString -> [Word8]
unpack = unpackBytes

fromChunks :: [S.ByteString] -> ByteString
fromChunks = List.foldr chunk Empty

toChunks :: ByteString -> [S.ByteString]
toChunks = foldrChunks (:) []

null :: ByteString -> Bool
null Empty = True
null _     = False

length :: ByteString -> Int64
length = foldlChunks (\n c -> n + primIntToInt64 (S.length c)) 0

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

cons :: Word8 -> ByteString -> ByteString
cons c = Chunk (S.singleton c)

cons' :: Word8 -> ByteString -> ByteString
cons' w (Chunk c cs) | S.length c < 16 = Chunk (S.cons w c) cs
cons' w cs                             = Chunk (S.singleton w) cs

snoc :: ByteString -> Word8 -> ByteString
snoc cs w = foldrChunks Chunk (singleton w) cs

head :: ByteString -> Word8
head Empty       = errorEmpty "head"
head (Chunk c _) = S.unsafeHead c

tail :: ByteString -> ByteString
tail Empty       = errorEmpty "tail"
tail (Chunk c cs)
  | S.length c == 1 = cs
  | otherwise       = Chunk (S.unsafeTail c) cs

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons Empty = Nothing
uncons (Chunk c cs) =
  case S.length c of
    1 -> Just (S.unsafeHead c, cs)
    _ -> Just (S.unsafeHead c, Chunk (S.unsafeTail c) cs)

last :: ByteString -> Word8
last Empty          = errorEmpty "last"
last (Chunk c0 cs0) = go c0 cs0
  where
    go c Empty        = S.unsafeLast c
    go _ (Chunk c cs) = go c cs

init :: ByteString -> ByteString
init Empty          = errorEmpty "init"
init (Chunk c0 cs0) = go c0 cs0
  where
    go c Empty
      | S.length c == 1 = Empty
      | otherwise       = Chunk (S.unsafeInit c) Empty
    go c (Chunk c' cs)  = Chunk c (go c' cs)

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc Empty = Nothing
unsnoc bs    = Just (init bs, last bs)

map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = go
    where
        go Empty        = Empty
        go (Chunk x xs) = Chunk y ys
            where
                y  = S.map f x
                ys = go xs

reverse :: ByteString -> ByteString
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk c cs) = rev (Chunk (S.reverse c) a) cs

intersperse :: Word8 -> ByteString -> ByteString
intersperse _ Empty        = Empty
intersperse w (Chunk c cs) = Chunk (S.intersperse w c) (foldrChunks (Chunk . intersperse') Empty cs)
  where
    intersperse' :: S.ByteString -> S.ByteString
    intersperse' = S.cons w . S.intersperse w

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = concat . List.intersperse s

transpose :: [ByteString] -> [ByteString]
transpose = List.map (\bs -> Chunk (S.pack bs) Empty) . List.transpose . List.map unpack

foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f = go
  where go a Empty        = a
        go a (Chunk c cs) = go (S.foldl f a c) cs

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (S.foldl' f a c) cs

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k = foldrChunks (flip (S.foldr k))

foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' f a = go
  where
    go Empty = a
    go (Chunk c cs) = S.foldr' f (foldr' f a cs) c

foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ Empty        = errorEmpty "foldl1"
foldl1 f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go v x xs = let v' = S.foldl f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' _ Empty        = errorEmpty "foldl1'"
foldl1' f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go !v x xs = let v' = S.foldl' f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ Empty          = errorEmpty "foldr1"
foldr1 f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1 f c
        go c (Chunk c' cs) = S.foldr  f (go c' cs) c

foldr1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' _ Empty          = errorEmpty "foldr1'"
foldr1' f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1' f c
        go c (Chunk c' cs) = S.foldr'  f (go c' cs) c

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap _ Empty          = Empty
concatMap f (Chunk c0 cs0) = to c0 cs0
  where
    go :: ByteString -> S.ByteString -> ByteString -> ByteString
    go Empty        c' cs' = to c' cs'
    go (Chunk c cs) c' cs' = Chunk c (go cs c' cs')

    to :: S.ByteString -> ByteString -> ByteString
    to c cs | S.null c  = case cs of
        Empty          -> Empty
        (Chunk c' cs') -> to c' cs'
            | otherwise = go (f (S.unsafeHead c)) (S.unsafeTail c) cs

any :: (Word8 -> Bool) -> ByteString -> Bool
any f = foldrChunks (\c rest -> S.any f c || rest) False

all :: (Word8 -> Bool) -> ByteString -> Bool
all f = foldrChunks (\c rest -> S.all f c && rest) True

maximum :: ByteString -> Word8
maximum Empty        = errorEmpty "maximum"
maximum (Chunk c cs) = foldlChunks (\n c' -> n `max` S.maximum c') (S.maximum c) cs

minimum :: ByteString -> Word8
minimum Empty        = errorEmpty "minimum"
minimum (Chunk c cs) = foldlChunks (\n c' -> n `min` S.minimum c') (S.minimum c) cs

compareLength :: ByteString -> Int64 -> Ordering
compareLength _ toCmp | toCmp < 0 = GT
compareLength Empty toCmp         = compare 0 toCmp
compareLength (Chunk c cs) toCmp  = compareLength cs (toCmp - primIntToInt64 (S.length c))

mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s',  c')  = S.mapAccumL f s c
              (s'', cs') = go s' cs

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s'', c') = S.mapAccumR f s' c
              (s', cs') = go s cs

scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanl f = fmap (uncurry (flip snoc)) . mapAccumL (\x y -> (f x y, x))

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 f bs = case uncons bs of
  Nothing -> Empty
  Just (b, bs') -> scanl f b bs'

scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanr f = fmap (uncurry cons) . mapAccumR (\x y -> (f y x, x))

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 f bs = case unsnoc bs of
  Nothing -> Empty
  Just (bs', b) -> scanr f b bs'

iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = unfoldr (\x -> case f x of !x' -> Just (x', x'))

repeat :: Word8 -> ByteString
repeat w = cs where cs = Chunk (S.replicate smallChunkSize w) cs

replicate :: Int64 -> Word8 -> ByteString
replicate n w
    | n <= 0             = Empty
    | n < primIntToInt64 smallChunkSize = Chunk (S.replicate (primInt64ToInt n) w) Empty
    | r == 0             = cs -- preserve invariant
    | otherwise          = Chunk (S.unsafeTake (primInt64ToInt r) c) cs
 where
    c      = S.replicate smallChunkSize w
    cs     = nChunks q
    (q, r) = quotRem n (primIntToInt64 smallChunkSize)
    nChunks 0 = Empty
    nChunks m = Chunk c (nChunks (m - 1))

cycle :: ByteString -> ByteString
cycle Empty = errorEmpty "cycle"
cycle bs    = bs' where bs' = bs `append` bs'

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = unfoldChunk 32
  where unfoldChunk n x =
          case S.unfoldrN n f x of
            (c, Nothing)
              | S.null c  -> Empty
              | otherwise -> Chunk c Empty
            (c, Just x')  -> Chunk c (unfoldChunk (n * 2) x')

take :: Int64 -> ByteString -> ByteString
take i _ | i <= 0 = Empty
take i cs0        = take' i cs0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk c cs) =
          if n < primIntToInt64 (S.length c)
            then Chunk (S.take (primInt64ToInt n) c) Empty
            else Chunk c (take' (n - primIntToInt64 (S.length c)) cs)

takeEnd :: Int64 -> ByteString -> ByteString
takeEnd i bs = drop (length bs - i) bs

drop :: Int64 -> ByteString -> ByteString
drop i p | i <= 0 = p
drop i cs0 = drop' i cs0
  where drop' 0 cs           = cs
        drop' _ Empty        = Empty
        drop' n (Chunk c cs) =
          if n < primIntToInt64 (S.length c)
            then Chunk (S.drop (primInt64ToInt n) c) cs
            else drop' (n - primIntToInt64 (S.length c)) cs

dropEnd :: Int64 -> ByteString -> ByteString
dropEnd i bs = take (length bs - i) bs

splitAt :: Int64 -> ByteString -> (ByteString, ByteString)
splitAt i cs0 | i <= 0 = (Empty, cs0)
splitAt i cs0 = splitAt' i cs0
  where splitAt' 0 cs           = (Empty, cs)
        splitAt' _ Empty        = (Empty, Empty)
        splitAt' n (Chunk c cs) =
          if n < primIntToInt64 (S.length c)
            then (Chunk (S.take (primInt64ToInt n) c) Empty
                 ,Chunk (S.drop (primInt64ToInt n) c) cs)
            else let (cs', cs'') = splitAt' (n - primIntToInt64 (S.length c)) cs
                   in (Chunk c cs', cs'')

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f = takeWhile'
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk c cs) =
          case S.findIndex (not . f) c of
            Just 0  -> Empty
            Just n  -> Chunk (S.take n c) Empty
            Nothing -> Chunk c (takeWhile' cs)

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f = takeWhileEnd'
  where takeWhileEnd' Empty = Empty
        takeWhileEnd' cs    =
            snd $ foldrChunks takeTuple (True,Empty) cs
        takeTuple _ (False, bs) = (False,bs)
        takeTuple c (True,bs)   =
           case S.takeWhileEnd f c of
                c' | S.length c' == S.length c -> (True, Chunk c bs)
                   | otherwise                 -> (False, fromStrict c' `append` bs)

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f = dropWhile'
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk c cs) =
          case S.findIndex (not . f) c of
            Just n  -> Chunk (S.drop n c) cs
            Nothing -> dropWhile' cs

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = go (c : acc) cs
            | otherwise    = List.foldl (flip Chunk) (go [] cs) (c : acc)
        go acc Empty       = dropEndBytes acc
        dropEndBytes []         = Empty
        dropEndBytes (x : xs)   =
            case S.dropWhileEnd f x of
                 x' | S.null x' -> dropEndBytes xs
                    | otherwise -> List.foldl' (flip Chunk) Empty (x' : xs)

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break f = break'
  where break' Empty        = (Empty, Empty)
        break' (Chunk c cs) =
          case S.findIndex f c of
            Just 0  -> (Empty, Chunk c cs)
            Just n  -> (Chunk (S.take n c) Empty, Chunk (S.drop n c) cs)
            Nothing -> let (cs', cs'') = break' cs
                       in (Chunk c cs', cs'')

breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = List.foldl (flip $ first . Chunk) (go [] cs) (c : acc)
            | otherwise = go (c : acc) cs
        go acc Empty = dropEndBytes acc
        dropEndBytes [] = (Empty, Empty)
        dropEndBytes (x : xs) =
            case S.breakEnd f x of
                 (x', x'') | S.null x' -> let (y, y') = dropEndBytes xs
                                           in (y, y' `append` fromStrict x)
                           | otherwise ->
                                List.foldl' (flip $ first . Chunk) (fromStrict x', fromStrict x'') xs
        first f (x, y) = (f x, y)

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = breakEnd (not . p)

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ Empty          = []
splitWith p (Chunk c0 cs0) = comb [] (S.splitWith p c0) cs0

  where comb :: [S.ByteString] -> [S.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.splitWith p c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict splitWith returned [] for nonempty input"

split :: Word8 -> ByteString -> [ByteString]
split _ Empty     = []
split w (Chunk c0 cs0) = comb [] (S.split w c0) cs0

  where comb :: [S.ByteString] -> [S.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.split w c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict split returned [] for nonempty input"

group :: ByteString -> [ByteString]
group = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndex (/= w) c of
        Just 0  -> revNonEmptyChunks acc : go (Chunk c cs)
        Nothing -> to (c : acc) w cs
        Just n  -> revNonEmptyChunks (S.unsafeTake n c : acc) : go (Chunk (S.unsafeDrop n c) cs)

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndex (not . k w) c of
        Just 0  -> revNonEmptyChunks acc : go (Chunk c cs)
        Nothing -> to (c : acc) w cs
        Just n  -> revNonEmptyChunks (S.unsafeTake n c : acc) : go (Chunk (S.unsafeDrop n c) cs)

index :: ByteString -> Int64 -> Word8
index _  i | i < 0  = error ("Data.ByteString.Lazy.index: negative index: " ++ show i)
index cs0 i         = index' cs0 i
  where index' Empty     n = error ("Data.ByteString.Lazy.index: index too large: " ++ show n)
        index' (Chunk c cs) n
          | n >= primIntToInt64 (S.length c) =
              index' cs (n - primIntToInt64 (S.length c))
          | otherwise       = S.unsafeIndex c (primInt64ToInt n)

indexMaybe :: ByteString -> Int64 -> Maybe Word8
indexMaybe _ i | i < 0 = Nothing
indexMaybe cs0 i       = index' cs0 i
  where index' Empty _ = Nothing
        index' (Chunk c cs) n
          | n >= primIntToInt64 (S.length c) =
              index' cs (n - primIntToInt64 (S.length c))
          | otherwise       = Just $! S.unsafeIndex c (primInt64ToInt n)

(!?) :: ByteString -> Int64 -> Maybe Word8
(!?) = indexMaybe

elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex w = elemIndex' 0
  where elemIndex' _ Empty        = Nothing
        elemIndex' n (Chunk c cs) =
          case S.elemIndex w c of
            Nothing -> elemIndex' (n + primIntToInt64 (S.length c)) cs
            Just i  -> Just (n + primIntToInt64 i)

elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
elemIndexEnd = findIndexEnd . (==)

elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices w = elemIndices' 0
  where elemIndices' _ Empty        = []
        elemIndices' n (Chunk c cs) = List.map ((+ n) . primIntToInt64) (S.elemIndices w c)
                             ++ elemIndices' (n + primIntToInt64 (S.length c)) cs

count :: Word8 -> ByteString -> Int64
count w = foldlChunks (\n c -> n + primIntToInt64 (S.count w c)) 0

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex k = findIndex' 0
  where findIndex' _ Empty        = Nothing
        findIndex' n (Chunk c cs) =
          case S.findIndex k c of
            Nothing -> findIndex' (n + primIntToInt64 (S.length c)) cs
            Just i  -> Just (n + primIntToInt64 i)

findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndexEnd k = findIndexEnd' 0
  where
    findIndexEnd' _ Empty = Nothing
    findIndexEnd' n (Chunk c cs) =
      let !n' = n + S.length c
          !i  = primIntToInt64 . (n +) <$> S.findIndexEnd k c
      in findIndexEnd' n' cs `mplus` i

find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f = find'
  where find' Empty        = Nothing
        find' (Chunk c cs) = case S.find f c of
            Nothing -> find' cs
            Just w  -> Just w

findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices k = findIndices' 0
  where findIndices' _ Empty        = []
        findIndices' n (Chunk c cs) = List.map ((+ n) . primIntToInt64) (S.findIndices k c)
                             ++ findIndices' (n + primIntToInt64 (S.length c)) cs

elem :: Word8 -> ByteString -> Bool
elem w bs =
  case elemIndex w bs of
    Nothing -> False
    _       -> True

notElem :: Word8 -> ByteString -> Bool
notElem w cs = not (w `elem` cs)

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p = go
    where
        go Empty        = Empty
        go (Chunk x xs) = chunk (S.filter p x) (go xs)

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition _ Empty = (Empty, Empty)
partition p (Chunk x xs) = (chunk t ts, chunk f fs)
  where
    (t,   f) = S.partition p x
    (ts, fs) = partition   p xs

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = x == y  && isPrefixOf xs ys
    | S.length x <  S.length y = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise                = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = S.splitAt (S.length y) x
        (yh,yt) = S.splitAt (S.length x) y

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix Empty bs  = Just bs
stripPrefix _ Empty  = Nothing
stripPrefix (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = if x == y then stripPrefix xs ys else Nothing
    | S.length x <  S.length y = do yt <- S.stripPrefix x y
                                    stripPrefix xs (Chunk yt ys)
    | otherwise                = do xt <- S.stripPrefix y x
                                    stripPrefix (Chunk xt xs) ys

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix x y = reverse <$> stripPrefix (reverse x) (reverse y)

zip :: ByteString -> ByteString -> [(Word8, Word8)]
zip = zipWith (,)

zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith _ Empty     _  = []
zipWith _ _      Empty = []
zipWith f (Chunk a as) (Chunk b bs) = go a as b bs
  where
    -- This loop is written in a slightly awkward way but ensures we
    -- don't have to allocate any 'Chunk' objects to pass to a recursive
    -- call.  We have in some sense performed SpecConstr manually.
    go !x xs !y ys = let
      -- Creating a thunk for reading one byte would
      -- be wasteful, so we evaluate these eagerly.
      -- See also #558 for a similar issue with uncons.
      !xHead = S.unsafeHead x
      !yHead = S.unsafeHead y
      in f xHead yHead : to (S.unsafeTail x) xs (S.unsafeTail y) ys

    to !x xs !y ys
      | Chunk x' xs' <- chunk x xs
      , Chunk y' ys' <- chunk y ys
      = go x' xs' y' ys'
      | otherwise = []

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith _ Empty _ = Empty
packZipWith _ _ Empty = Empty
packZipWith f (Chunk a as) (Chunk b bs) = Chunk (S.packZipWith f a b) $
    case compare al bl of
        LT -> packZipWith f as $ Chunk (S.drop al b) bs
        EQ -> packZipWith f as bs
        GT -> packZipWith f (Chunk (S.drop bl a) as) bs
  where
    al = S.length a
    bl = S.length b

unzip :: [(Word8, Word8)] -> (ByteString, ByteString)
unzip ls = (pack (List.map fst ls), pack (List.map snd ls))

inits :: ByteString -> [ByteString]
inits bs = NE.toList $! initsNE bs

initsNE :: ByteString -> NonEmpty ByteString
initsNE = (Empty :|) . inits' id
  where
    inits' :: (ByteString -> ByteString) -> ByteString -> [ByteString]
    -- inits' f bs === map f (tail (inits bs))
    inits' _ Empty = []
    inits' f (Chunk c cs)
      = [f (S.unsafeTake n c `Chunk` Empty) | n <- [1..S.length c]]
      ++ inits' (f . Chunk c) cs

tails :: ByteString -> [ByteString]
tails bs = NE.toList $! tailsNE bs

tailsNE :: ByteString -> NonEmpty ByteString
tailsNE bs = case uncons bs of
  Nothing -> Empty :| []
  Just (_, tl) -> bs :| tails tl

copy :: ByteString -> ByteString
copy = foldrChunks (Chunk . S.copy) Empty

hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k -- only blocks if there is no data available
        if S.null c
          then hClose h >> return Empty
          else Chunk c <$> lazyRead

hGetN :: Int -> Handle -> Int -> IO ByteString
hGetN k h n | n > 0 = readChunks n
  where
    readChunks !i = do
        c <- S.hGet h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)
hGetN _ _ 0 = return Empty
hGetN _ h n = illegalBufferSize h "hGet" n

hGetNonBlockingN :: Int -> Handle -> Int -> IO ByteString
hGetNonBlockingN k h n | n > 0 = readChunks n
  where
    readChunks !i = do
        c <- S.hGetNonBlocking h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)
hGetNonBlockingN _ _ 0 = return Empty
hGetNonBlockingN _ h n = illegalBufferSize h "hGetNonBlocking" n

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError InvalidArgument msg (Just handle) Nothing)
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = hGetNonBlockingN defaultChunkSize

readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

getContents :: IO ByteString
getContents = hGetContents stdin

hPut :: Handle -> ByteString -> IO ()
hPut h = foldrChunks (\c rest -> S.hPut h c >> rest) (return ())

hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking _ Empty           = return Empty
hPutNonBlocking h bs@(Chunk c cs) = do
  c' <- S.hPutNonBlocking h c
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking h cs
    0                     -> return bs
    _                     -> return (Chunk c' cs)

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

putStr :: ByteString -> IO ()
putStr = hPut stdout

interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- ---------------------------------------------------------------------
-- Internal utilities

errorEmpty :: String -> a
errorEmpty fun = error ("Data.ByteString.Lazy." ++ fun ++ ": empty bytestring")

-- reverse a list of non-empty chunks into a lazy ByteString
revNonEmptyChunks :: [S.ByteString] -> ByteString
revNonEmptyChunks = List.foldl' (flip Chunk) Empty

-- reverse a list of possibly-empty chunks into a lazy ByteString
revChunks :: [S.ByteString] -> ByteString
revChunks = List.foldl' (flip chunk) Empty
