-- |
-- Module      : Data.ByteString.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate 'ByteString's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.ByteString".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
--
-- See:
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Char8 (

        -- * The @ByteString@ type
        ByteString,

        -- * Introducing and eliminating 'ByteString's
        B.empty,
        singleton,
        pack,
        unpack,
        B.fromStrict,
        B.toStrict,

        -- * Basic interface
        cons,
        snoc,
        B.append,
        head,
        uncons,
        unsnoc,
        last,
        B.tail,
        B.init,
        B.null,
        B.length,

        -- * Transforming ByteStrings
        map,
        B.reverse,
        intersperse,
        B.intercalate,
        B.transpose,

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
        B.concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Generating and unfolding ByteStrings
        replicate,
        unfoldr,
        unfoldrN,

        -- * Substrings

        -- ** Breaking strings
        B.take,
        B.takeEnd,
        B.drop,
        B.dropEnd,
        B.splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        dropSpace,
        span,
        spanEnd,
        break,
        breakEnd,
        B.group,
        groupBy,
        B.inits,
        B.tails,
        B.initsNE,
        B.tailsNE,
        strip,
        B.stripPrefix,
        B.stripSuffix,

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- ** Breaking into lines and words
        lines,
        words,
        unlines,
        unwords,

        -- * Predicates
        B.isPrefixOf,
        B.isSuffixOf,
        B.isInfixOf,

        -- ** Search for arbitrary substrings
        B.breakSubstring,

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
        elemIndices,
        elemIndexEnd,
        findIndex,
        findIndices,
        findIndexEnd,
        count,

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
        B.sort,

        -- * Reading from ByteStrings
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

        -- * Low level CString conversions

        -- ** Copying ByteStrings
        B.copy,

        -- ** Packing CStrings and pointers
        B.packCString,
        B.packCStringLen,

        -- ** Using ByteStrings as CStrings
        B.useAsCString,
        B.useAsCStringLen,

        -- * I\/O with 'ByteString's
        -- | ByteString I/O uses binary mode, without any character decoding
        -- or newline conversion. The fact that it does not respect the Handle
        -- newline mode is considered a flaw and may be changed in a future version.

        -- ** Standard input and output
        B.getLine,
        B.getContents,
        B.putStr,
        putStrLn,
        B.interact,

        -- ** Files
        B.readFile,
        B.writeFile,
        B.appendFile,
--      mmapFile,

        -- ** I\/O with Handles
        B.hGetLine,
        B.hGetContents,
        B.hGet,
        B.hGetSome,
        B.hGetNonBlocking,
        B.hPut,
        B.hPutNonBlocking,
        B.hPutStr,
        hPutStrLn,

  ) where

import Primitives

import qualified Prelude ()
import MiniPrelude as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Word.Word8 (Word8)
import System.IO.Base (Handle, stdout)

singleton :: Char -> ByteString
singleton = B.singleton . c2w

pack :: String -> ByteString
pack = B.pack . P.map c2w

unpack :: ByteString -> [Char]
unpack = P.map w2c . B.unpack

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

cons :: Char -> ByteString -> ByteString
cons = B.cons . c2w

snoc :: ByteString -> Char -> ByteString
snoc p = B.snoc p . c2w

uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case B.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')

unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs = case B.unsnoc bs of
                  Nothing -> Nothing
                  Just (bs', w) -> Just (bs', w2c w)

head :: ByteString -> Char
head = w2c . B.head

last :: ByteString -> Char
last = w2c . B.last

map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (c2w . f . w2c)

intersperse :: Char -> ByteString -> ByteString
intersperse = B.intersperse . c2w

foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = B.foldl (\a c -> f a (w2c c))

foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = B.foldl' (\a c -> f a (w2c c))

foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = B.foldr (f . w2c)

foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = B.foldr' (f . w2c)

foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (B.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (B.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (B.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)

any :: (Char -> Bool) -> ByteString -> Bool
any f = B.any (f . w2c)

all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)

maximum :: ByteString -> Char
maximum = w2c . B.maximum

minimum :: ByteString -> Char
minimum = w2c . B.minimum

mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = B.mapAccumL (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = B.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = B.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = B.scanl1 (\a b -> c2w (f (w2c a) (w2c b)))

scanr :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanr f z = B.scanr (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = B.scanr1 (\a b -> c2w (f (w2c a) (w2c b)))

replicate :: Int -> Char -> ByteString
replicate n = B.replicate n . c2w

unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f = B.unfoldr (fmap k . f)
    where k (i, j) = (c2w i, j)

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f = B.unfoldrN n ((k `fmap`) . f)
    where k (i,j) = (c2w i, j)

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)

takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd f = B.takeWhileEnd (f . w2c)

dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = B.dropWhileEnd (f . w2c)

break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)

breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c p = case elemIndex c p of
    Nothing -> (p, B.empty)
    Just n  -> (B.unsafeTake n p, B.unsafeDrop n p)

span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)

spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = B.spanEnd (f . w2c)

breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = B.breakEnd (f . w2c)

split :: Char -> ByteString -> [ByteString]
split = B.split . c2w

splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = B.splitWith (f . w2c)

groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = B.groupBy (\a b -> k (w2c a) (w2c b))

index :: ByteString -> Int -> Char
index = (w2c .) . B.index

indexMaybe :: ByteString -> Int -> Maybe Char
indexMaybe = (fmap w2c .) . B.indexMaybe

(!?) :: ByteString -> Int -> Maybe Char
(!?) = indexMaybe

elemIndex :: Char -> ByteString -> Maybe Int
elemIndex = B.elemIndex . c2w

elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w

elemIndices :: Char -> ByteString -> [Int]
elemIndices = B.elemIndices . c2w

findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex f = B.findIndex (f . w2c)

findIndexEnd :: (Char -> Bool) -> ByteString -> Maybe Int
findIndexEnd f = B.findIndexEnd (f . w2c)

findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices f = B.findIndices (f . w2c)

count :: Char -> ByteString -> Int
count c = B.count (c2w c)

elem :: Char -> ByteString -> Bool
elem    c = B.elem (c2w c)

notElem :: Char -> ByteString -> Bool
notElem c = B.notElem (c2w c)

filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = B.filter (f . w2c)

partition :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
partition f = B.partition (f . w2c)

find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` B.find (f . w2c) ps

zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> (psH, qsH) : zip psT qsT

zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = B.zipWith ((. w2c) . f . w2c)

packZipWith :: (Char -> Char -> Char) -> ByteString -> ByteString -> ByteString
packZipWith f = B.packZipWith f'
    where
        f' c1 c2 = c2w $ f (w2c c1) (w2c c2)

unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))

dropSpace :: ByteString -> ByteString
dropSpace = dropWhile isSpace

strip :: ByteString -> ByteString
strip = dropWhile isSpace . dropWhileEnd isSpace

lines :: ByteString -> [ByteString]
lines ps
    | B.null ps = []
    | otherwise = case elemIndex '\n' ps of
             Nothing -> [ps]
             Just n  -> B.take n ps : lines (B.drop (n+1) ps)

unlines :: [ByteString] -> ByteString
unlines = B.intercalate (singleton '\n')

words :: ByteString -> [ByteString]
words = P.filter (not . B.null) . splitWith isSpace

unwords :: [ByteString] -> ByteString
unwords = B.intercalate (singleton ' ')

hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps
    | B.length ps < 1024 = B.hPut h (ps `B.snoc` 0x0a)
    | otherwise        = B.hPut h ps >> B.hPut h (B.singleton 0x0a) -- don't copy

putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout
