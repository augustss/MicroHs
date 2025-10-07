-- |
-- Module      : Data.ByteString.Lazy.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate /lazy/ 'ByteString's using 'Char' operations. All Chars will
-- be truncated to 8 bits. It can be expected that these functions will
-- run at identical speeds to their 'Data.Word.Word8' equivalents in
-- "Data.ByteString.Lazy".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Lazy.Char8 (

        -- * The @ByteString@ type
        ByteString,

        -- * Introducing and eliminating 'ByteString's
        L.empty,
        singleton,
        pack,
        unpack,
        L.fromChunks,
        L.toChunks,
        L.fromStrict,
        L.toStrict,

        -- * Basic interface
        cons,
        --cons',
        snoc,
        L.append,
        head,
        uncons,
        last,
        L.tail,
        unsnoc,
        L.init,
        L.null,
        L.length,

        -- * Transforming ByteStrings
        map,
        L.reverse,
        intersperse,
        L.intercalate,
        L.transpose,

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
        L.concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,
        L.compareLength,

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
        L.cycle,
        iterate,

        -- ** Unfolding ByteStrings
        unfoldr,

        -- * Substrings

        -- ** Breaking strings
        L.take,
        L.takeEnd,
        L.drop,
        L.dropEnd,
        L.splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        span,
        spanEnd,
        break,
        breakEnd,
        L.group,
        groupBy,
        L.inits,
        L.tails,
        L.initsNE,
        L.tailsNE,
        L.stripPrefix,
        L.stripSuffix,

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- ** Breaking into lines and words
        lines,
        words,
        unlines,
        unwords,

        -- * Predicates
        L.isPrefixOf,
        L.isSuffixOf,

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
        L.copy,

        -- * Reading from ByteStrings
        -- | Note that a lazy 'ByteString' may hold an unbounded stream of
        -- @\'0\'@ digits, in which case the functions below may never return.
        -- If that's a concern, you can use 'take' to first truncate the input
        -- to an acceptable length.  Non-termination is also possible when
        -- reading arbitrary precision numbers via 'readInteger' or
        -- 'readNatural', if the input is an unbounded stream of arbitrary
        -- decimal digits.
        --
        --readInt,
        --readInt64,
        --readInt32,
        --readInt16,
        --readInt8,

        --readWord,
        --readWord64,
        --readWord32,
        --readWord16,
        --readWord8,

        --readInteger,
        --readNatural,

        -- * I\/O with 'ByteString's
        -- | ByteString I/O uses binary mode, without any character decoding
        -- or newline conversion. The fact that it does not respect the Handle
        -- newline mode is considered a flaw and may be changed in a future version.

        -- ** Standard input and output
        L.getContents,
        L.putStr,
        putStrLn,
        L.interact,

        -- ** Files
        L.readFile,
        L.writeFile,
        L.appendFile,

        -- ** I\/O with Handles
        L.hGetContents,
        L.hGet,
        L.hGetNonBlocking,
        L.hPut,
        L.hPutNonBlocking,
        L.hPutStr,
        hPutStrLn,

  ) where

import qualified Prelude ()
import MiniPrelude as P
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S (ByteString) -- typename only
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.List.NonEmpty (NonEmpty(..))

import Data.ByteString.Internal (c2w, w2c)

import Data.Int (Int64)
import qualified Data.List as List

import System.IO.Base (Handle, stdout)

------------------------------------------------------------------------

singleton :: Char -> ByteString
singleton = L.singleton . c2w

pack :: [Char] -> ByteString
pack = L.pack . P.map c2w

unpack :: ByteString -> [Char]
unpack = P.map w2c . L.unpack

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

cons :: Char -> ByteString -> ByteString
cons = L.cons . c2w

--cons' :: Char -> ByteString -> ByteString
--cons' = L.cons' . c2w

snoc :: ByteString -> Char -> ByteString
snoc p = L.snoc p . c2w

head :: ByteString -> Char
head = w2c . L.head

uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case L.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')

unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs = case L.unsnoc bs of
                  Nothing -> Nothing
                  Just (bs', w) -> Just (bs', w2c w)

last :: ByteString -> Char
last = w2c . L.last

map :: (Char -> Char) -> ByteString -> ByteString
map f = L.map (c2w . f . w2c)

intersperse :: Char -> ByteString -> ByteString
intersperse = L.intersperse . c2w

foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = L.foldl (\a c -> f a (w2c c))

foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = L.foldl' (\a c -> f a (w2c c))

foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = L.foldr (f . w2c)

foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = L.foldr' (f . w2c)

foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (L.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (L.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (L.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (L.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = L.concatMap (f . w2c)

any :: (Char -> Bool) -> ByteString -> Bool
any f = L.any (f . w2c)

all :: (Char -> Bool) -> ByteString -> Bool
all f = L.all (f . w2c)

maximum :: ByteString -> Char
maximum = w2c . L.maximum

minimum :: ByteString -> Char
minimum = w2c . L.minimum

scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = L.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = L.scanl1 f'
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

scanr
    :: (Char -> Char -> Char)
    -- ^ element -> accumulator -> new accumulator
    -> Char
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr f = L.scanr f' . c2w
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = L.scanr1 f'
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = L.mapAccumL (\a w -> case f a (w2c w) of (a',c) -> (a', c2w c))

mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = L.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

iterate :: (Char -> Char) -> Char -> ByteString
iterate f = L.iterate (c2w . f . w2c) . c2w

repeat :: Char -> ByteString
repeat = L.repeat . c2w

replicate :: Int64 -> Char -> ByteString
replicate w c = L.replicate w (c2w c)

unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f = L.unfoldr $ \a -> case f a of
                                    Nothing      -> Nothing
                                    Just (c, a') -> Just (c2w c, a')

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = L.takeWhile (f . w2c)

takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd f = L.takeWhileEnd (f . w2c)

dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = L.dropWhile (f . w2c)

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = L.dropWhileEnd (f . w2c)

break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = L.break (f . w2c)

breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = L.breakEnd (f . w2c)

span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = L.span (f . w2c)

spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = L.spanEnd (f . w2c)

split :: Char -> ByteString -> [ByteString]
split = L.split . c2w

splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = L.splitWith (f . w2c)

groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = L.groupBy (\a b -> k (w2c a) (w2c b))

index :: ByteString -> Int64 -> Char
index = (w2c .) . L.index

indexMaybe :: ByteString -> Int64 -> Maybe Char
indexMaybe = (fmap w2c .) . L.indexMaybe

(!?) :: ByteString -> Int64 -> Maybe Char
(!?) = indexMaybe

elemIndex :: Char -> ByteString -> Maybe Int64
elemIndex = L.elemIndex . c2w

elemIndexEnd :: Char -> ByteString -> Maybe Int64
elemIndexEnd = L.elemIndexEnd . c2w

elemIndices :: Char -> ByteString -> [Int64]
elemIndices = L.elemIndices . c2w

findIndex :: (Char -> Bool) -> ByteString -> Maybe Int64
findIndex f = L.findIndex (f . w2c)

findIndexEnd :: (Char -> Bool) -> ByteString -> Maybe Int64
findIndexEnd f = L.findIndexEnd (f . w2c)

findIndices :: (Char -> Bool) -> ByteString -> [Int64]
findIndices f = L.findIndices (f . w2c)

count :: Char -> ByteString -> Int64
count c = L.count (c2w c)

elem :: Char -> ByteString -> Bool
elem c = L.elem (c2w c)

notElem :: Char -> ByteString -> Bool
notElem c = L.notElem (c2w c)

filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = L.filter (f . w2c)

partition :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
partition f = L.partition (f . w2c)

find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` L.find (f . w2c) ps

zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | L.null ps || L.null qs = []
    | otherwise = (head ps, head qs) : zip (L.tail ps) (L.tail qs)

zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = L.zipWith ((. w2c) . f . w2c)

packZipWith :: (Char -> Char -> Char) -> ByteString -> ByteString -> ByteString
packZipWith f = L.packZipWith f'
    where
        f' c1 c2 = c2w $ f (w2c c1) (w2c c2)

unzip :: [(Char, Char)] -> (ByteString, ByteString)
unzip ls = (pack (fmap fst ls), pack (fmap snd ls))

lines :: ByteString -> [ByteString]
lines bs
  | L.null bs = []
  | otherwise = case elemIndex '\n' bs of
      Nothing -> [bs]
      Just n  -> L.take n bs : lines (L.drop (n + 1) bs)

unlines :: [ByteString] -> ByteString
unlines = List.foldr (\x t -> x `L.append` cons '\n' t) L.empty

words :: ByteString -> [ByteString]
words = List.filter (not . L.null) . splitWith isSpace

unwords :: [ByteString] -> ByteString
unwords = L.intercalate (singleton ' ')

hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps = L.hPut h ps >> L.hPut h (L.singleton 0x0a)

putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout
