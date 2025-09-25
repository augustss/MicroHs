{-

API matches that of

-- Package     : bytestring-0.12.2.0
--
-- Module      : Data.ByteString.Short.Internal
-- Copyright   : (c) Duncan Coutts 2012-2013, Julian Ospald 2022
-- License     : BSD-style
--
-- Maintainer  : hasufell@posteo.de
-- Stability   : stable
-- Portability : ghc only

but with the representation of ShortByteString hidden
(it is different from GHC).

types and documentation below are copied from bytestring-0.12.2.0
though the implementations differ (except where stated)

-}
module Data.ByteString.Short.Internal(
    -- * The @ShortByteString@ type and representation
    ShortByteString,

    -- * Introducing and eliminating 'ShortByteString's
    empty,
    singleton,
    pack,
    unpack,
    fromShort,
    toShort,

    -- * Basic interface
    snoc,
    cons,
    append,
    last,
    tail,
    uncons,
    head,
    init,
    unsnoc,
    null,
    length,

    -- * Transforming ShortByteStrings
    map,
    reverse,
    intercalate,

    -- * Reducing 'ShortByteString's (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',

    foldr,
    foldr',
    foldr1,
    foldr1',

    -- ** Special folds
    all,
    any,
    concat,

    -- ** Generating and unfolding ShortByteStrings
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    takeWhileEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    dropWhileEnd,
    breakEnd,
    break,
    span,
    spanEnd,
    splitAt,
    split,
    splitWith,
    stripSuffix,
    stripPrefix,

    -- * Predicates
    isInfixOf,
    isPrefixOf,
    isSuffixOf,

    -- ** Search for arbitrary substrings
    breakSubstring,

    -- * Searching ShortByteStrings

    -- ** Searching by equality
    elem,

    -- ** Searching with a predicate
    find,
    filter,
    partition,

    -- * Indexing ShortByteStrings
    index,
    indexMaybe,
    (!?),
    elemIndex,
    elemIndices,
    count,
    findIndex,
    findIndices,
    unsafeIndex,

{-
    -- * Low level operations -- XXX
    createFromPtr,
    copyToPtr,
-}

    -- ** Encoding validation
    isValidUtf8,

    -- * Low level conversions
    -- ** Packing 'Foreign.C.String.CString's and pointers
    packCString,
    packCStringLen,

    -- ** Using ShortByteStrings as 'Foreign.C.String.CString's
    useAsCString,
    useAsCStringLen
  ) where

import Prelude hiding
  ( empty, singleton, pack, unpack, fromShort, toShort,
    snoc, cons, append, unsnoc, last, tail, uncons, head, init,
    null, length, map, reverse, intercalate,
    foldl, foldl', foldl1, foldl1', foldr, foldr', foldr1, foldr1',
    all, any, concat, replicate, unfoldr, unfoldrN,
    take, takeEnd, takeWhileEnd, takeWhile,
    drop, dropEnd, dropWhileEnd, dropWhile,
    break, breakEnd, span, spanEnd, splitAt, split, splitWith,
    stripSuffix, stripPrefix, isInfixOf, isPrefixOf, isSuffixOf,
    breakSubstring, elem, find, filter, partition,
    index, indexMaybe, (!?), elemIndex, elemIndices, count,
    findIndex, findIndices, unsafeIndex,
--  createFromPtr, copyToPtr, -- XXX
    isValidUtf8,
    packCString, packCStringLen, useAsCString, useAsCStringLen,
  )

import Foreign (Ptr)
import Foreign.C.String (CString, CStringLen)

import Data.Data ( Data(..) )
import Data.Typeable ( Typeable(..) )
import GHC.Generics ( Generic(..) )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..), stimesMonoid )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.String ( IsString(..) )
-- import qualified GHC.Exts -- XXX OverloadedLists
import GHC.Stack ( HasCallStack )
import Data.ByteString as BS
import Data.ByteString.Unsafe as BS.Unsafe

import Data.Word (Word8)

newtype ShortByteString = S ByteString
  deriving (Eq, Ord, Data, Typeable, Generic)

fromShort :: ShortByteString -> ByteString
fromShort (S b) = b

toShort :: ByteString -> ShortByteString
toShort b = S b

-- copied from bytestring-0.12.2.0
instance Semigroup ShortByteString where
  (<>)    = append
  sconcat (b:|bs) = concat (b:bs)
  stimes  = stimesMonoid

-- copied from bytestring-0.12.2.0
instance Monoid ShortByteString where
  mempty  = empty
  mappend = (<>)
  mconcat = concat

-- copied from bytestring-0.12.2.0
instance Show ShortByteString where
  showsPrec p ps r = showsPrec p (unpackChars ps) r

-- copied from bytestring-0.12.2.0
instance Read ShortByteString where
  readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

-- copied from bytestring-0.12.2.0
instance IsString ShortByteString where
  fromString = packChars

{-
instance GHC.Exts.IsList ShortByteString where -- XXX OverloadedLists
  type Item ShortByteString = Word8
  fromList  = toShort . GHC.Exts.fromList
  fromListN = (toShort .) . GHC.Exts.fromListN
  toList    = GHC.Exts.toList . fromShort
-}

empty :: ShortByteString
empty = toShort BS.empty

singleton :: Word8 -> ShortByteString
singleton = toShort . BS.singleton

pack :: [Word8] -> ShortByteString
pack = toShort . BS.pack

unpack :: ShortByteString -> [Word8]
unpack = BS.unpack . fromShort

snoc :: ShortByteString -> Word8 -> ShortByteString
snoc s c = toShort (BS.snoc (fromShort s) c)

cons :: Word8 -> ShortByteString -> ShortByteString
cons c s = toShort (BS.cons c (fromShort s))

append :: ShortByteString -> ShortByteString -> ShortByteString
append s t = toShort (BS.append (fromShort s) (fromShort t))

last :: HasCallStack => ShortByteString -> Word8
last = BS.last . fromShort

tail :: HasCallStack => ShortByteString -> ShortByteString
tail = toShort . BS.tail . fromShort

uncons :: ShortByteString -> Maybe (Word8, ShortByteString)
uncons = fmap f . BS.uncons . fromShort
  where f (c, s) = (c, toShort s)

head :: HasCallStack => ShortByteString -> Word8
head = BS.head . fromShort

init :: HasCallStack => ShortByteString -> ShortByteString
init = toShort . BS.init . fromShort

unsnoc :: ShortByteString -> Maybe (ShortByteString, Word8)
unsnoc = fmap f . BS.unsnoc . fromShort
  where f (s, c) = (toShort s, c)

null :: ShortByteString -> Bool
null = BS.null . fromShort

length :: ShortByteString -> Int
length = BS.length . fromShort

map :: (Word8 -> Word8) -> ShortByteString -> ShortByteString
map f = toShort . BS.map f . fromShort

reverse :: ShortByteString -> ShortByteString
reverse = toShort . BS.reverse . fromShort

intercalate :: ShortByteString -> [ShortByteString] -> ShortByteString
intercalate s = toShort . BS.intercalate (fromShort s) . fmap fromShort

foldl :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl f e = BS.foldl f e . fromShort

foldl' :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl' f e = BS.foldl' f e . fromShort

foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1 f = BS.foldl1 f . fromShort

foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1' f = BS.foldl1' f . fromShort

foldr :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr f e = BS.foldr f e . fromShort

foldr' :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr' f e = BS.foldr' f e . fromShort

foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1 f = BS.foldr1 f . fromShort

foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1' f = BS.foldr1' f . fromShort

all :: (Word8 -> Bool) -> ShortByteString -> Bool
all f = BS.all f . fromShort

any :: (Word8 -> Bool) -> ShortByteString -> Bool
any f = BS.any f . fromShort

concat :: [ShortByteString] -> ShortByteString
concat = toShort . BS.concat . fmap fromShort

replicate :: Int -> Word8 -> ShortByteString
replicate n = toShort . BS.replicate n

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ShortByteString
unfoldr f = toShort . BS.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ShortByteString, Maybe a)
unfoldrN n f a = case BS.unfoldrN n f a of (s, ma) -> (toShort s, ma)

take :: Int -> ShortByteString -> ShortByteString
take n = toShort . BS.take n . fromShort

takeEnd :: Int -> ShortByteString -> ShortByteString
takeEnd n = toShort . BS.takeEnd n . fromShort

takeWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhileEnd f = toShort . BS.takeWhileEnd f . fromShort

takeWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhile f = toShort . BS.takeWhile f . fromShort

drop :: Int -> ShortByteString -> ShortByteString
drop n = toShort . BS.drop n . fromShort

dropEnd :: Int -> ShortByteString -> ShortByteString
dropEnd n = toShort . BS.dropEnd n . fromShort

dropWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhile f = toShort . BS.dropWhile f . fromShort

dropWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd f = toShort . BS.dropWhileEnd f . fromShort

breakEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd f s = case BS.breakEnd f (fromShort s) of (a, b) -> (toShort a, toShort b)

break :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break f s = case BS.break f (fromShort s) of (a, b) -> (toShort a, toShort b)

span :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
span f s = case BS.span f (fromShort s) of (a, b) -> (toShort a, toShort b)

spanEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd f s = case BS.spanEnd f (fromShort s) of (a, b) -> (toShort a, toShort b)

splitAt :: Int -> ShortByteString -> (ShortByteString, ShortByteString)
splitAt n s = case BS.splitAt n (fromShort s) of (a, b) -> (toShort a, toShort b)

split :: Word8 -> ShortByteString -> [ShortByteString]
split c = fmap toShort . BS.split c . fromShort

splitWith :: (Word8 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith f = fmap toShort . BS.splitWith f . fromShort

stripSuffix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripSuffix s = fmap toShort . BS.stripSuffix (fromShort s) . fromShort

stripPrefix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripPrefix s = fmap toShort . BS.stripPrefix (fromShort s) . fromShort

isInfixOf :: ShortByteString -> ShortByteString -> Bool
isInfixOf s t = BS.isInfixOf (fromShort s) (fromShort t)

isPrefixOf :: ShortByteString -> ShortByteString -> Bool
isPrefixOf s t = BS.isPrefixOf (fromShort s) (fromShort t)

isSuffixOf :: ShortByteString -> ShortByteString -> Bool
isSuffixOf s t = BS.isSuffixOf (fromShort s) (fromShort t)

breakSubstring :: ShortByteString -> ShortByteString -> (ShortByteString, ShortByteString)
breakSubstring s t = case BS.breakSubstring (fromShort s) (fromShort t) of (a, b) -> (toShort a, toShort b)

elem :: Word8 -> ShortByteString -> Bool
elem c = BS.elem c . fromShort

find :: (Word8 -> Bool) -> ShortByteString -> Maybe Word8
find f = BS.find f . fromShort

filter :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
filter f = toShort . BS.filter f . fromShort

partition :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
partition f s = case BS.partition f (fromShort s) of (a, b) -> (toShort a, toShort b)

index :: HasCallStack => ShortByteString -> Int -> Word8
index = BS.index . fromShort

indexMaybe :: ShortByteString -> Int -> Maybe Word8
indexMaybe = BS.indexMaybe . fromShort

(!?) :: ShortByteString -> Int -> Maybe Word8
s !? i = fromShort s BS.!? i

elemIndex :: Word8 -> ShortByteString -> Maybe Int
elemIndex c = BS.elemIndex c . fromShort

elemIndices :: Word8 -> ShortByteString -> [Int]
elemIndices c = BS.elemIndices c . fromShort

count :: Word8 -> ShortByteString -> Int
count c = BS.count c . fromShort

findIndex :: (Word8 -> Bool) -> ShortByteString -> Maybe Int
findIndex f = BS.findIndex f . fromShort

findIndices :: (Word8 -> Bool) -> ShortByteString -> [Int]
findIndices f = BS.findIndices f . fromShort

unsafeIndex :: ShortByteString -> Int -> Word8
unsafeIndex = BS.Unsafe.unsafeIndex . fromShort

{- XXX
createFromPtr :: Ptr a -> Int -> IO ShortByteString
createFromPtr p n = fmap toShort (BS.createFromPtr p n)

copyToPtr :: ShortByteString -> Int -> Ptr a -> Int -> IO ()
copyToPtr = BS.copyToPtr . fromShort
-}

isValidUtf8 :: ShortByteString -> Bool
isValidUtf8 = BS.isValidUtf8 . fromShort

packCString :: CString -> IO ShortByteString
packCString s = fmap toShort (BS.packCString s)

packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen s = fmap toShort (BS.packCStringLen s)

useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString = BS.useAsCString . fromShort

useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen = BS.useAsCStringLen . fromShort

-- private

-- truncates wide characters
packChars :: [Char] -> ShortByteString
packChars = toShort . BS.pack . fmap (toEnum . fromEnum)

unpackChars :: ShortByteString -> [Char]
unpackChars = fmap  (toEnum . fromEnum) . BS.unpack . fromShort
