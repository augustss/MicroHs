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
import Control.DeepSeq.Class
import Foreign (Ptr)
import Foreign.C.String (CString, CStringLen)

import Data.Coerce ( coerce )
import Data.Data ( Data(..) )
import Data.Typeable ( Typeable(..) )
import GHC.Generics ( Generic(..) )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..), stimesMonoid )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.String ( IsString(..) )
-- import qualified GHC.Exts -- XXX OverloadedLists
import Data.ByteString as BS
import Data.ByteString.Unsafe as BS.Unsafe

import Data.Word (Word8)

newtype ShortByteString = S ByteString
--  deriving (Eq, Ord, Data, Typeable, Generic)
  deriving (Eq, Ord, Typeable, NFData)

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
empty = coerce BS.empty

singleton :: Word8 -> ShortByteString
singleton = coerce BS.singleton

pack :: [Word8] -> ShortByteString
pack = coerce BS.pack

unpack :: ShortByteString -> [Word8]
unpack = coerce BS.unpack

snoc :: ShortByteString -> Word8 -> ShortByteString
snoc = coerce BS.snoc

cons :: Word8 -> ShortByteString -> ShortByteString
cons = coerce BS.cons

append :: ShortByteString -> ShortByteString -> ShortByteString
append = coerce BS.append

last :: {-HasCallStack => -} ShortByteString -> Word8
last = coerce BS.last

tail :: {-HasCallStack => -} ShortByteString -> ShortByteString
tail = coerce BS.tail

uncons :: ShortByteString -> Maybe (Word8, ShortByteString)
uncons = coerce BS.uncons

head :: {-HasCallStack => -} ShortByteString -> Word8
head = coerce BS.head

init :: {-HasCallStack => -} ShortByteString -> ShortByteString
init = coerce BS.init

unsnoc :: ShortByteString -> Maybe (ShortByteString, Word8)
unsnoc = coerce BS.unsnoc

null :: ShortByteString -> Bool
null = coerce BS.null

length :: ShortByteString -> Int
length = coerce BS.length

map :: (Word8 -> Word8) -> ShortByteString -> ShortByteString
map = coerce BS.map

reverse :: ShortByteString -> ShortByteString
reverse = coerce BS.reverse

intercalate :: ShortByteString -> [ShortByteString] -> ShortByteString
intercalate = coerce BS.intercalate

foldl :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl f e = BS.foldl f e . coerce

foldl' :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl' f e = BS.foldl' f e . coerce

foldl1 :: {- HasCallStack => -} (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1 = coerce BS.foldl1

foldl1' :: {- HasCallStack => -} (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1' = coerce BS.foldl1'

foldr :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr f e = BS.foldr f e . coerce

foldr' :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr' f e = BS.foldr' f e . coerce

foldr1 :: {- HasCallStack => -} (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1 = coerce BS.foldr1

foldr1' :: {- HasCallStack => -} (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1' = coerce BS.foldr1'

all :: (Word8 -> Bool) -> ShortByteString -> Bool
all = coerce BS.all

any :: (Word8 -> Bool) -> ShortByteString -> Bool
any = coerce BS.any

concat :: [ShortByteString] -> ShortByteString
concat = coerce BS.concat

replicate :: Int -> Word8 -> ShortByteString
replicate = coerce BS.replicate

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ShortByteString
unfoldr f = coerce . BS.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ShortByteString, Maybe a)
unfoldrN n f = coerce . BS.unfoldrN n f

take :: Int -> ShortByteString -> ShortByteString
take = coerce BS.take

takeEnd :: Int -> ShortByteString -> ShortByteString
takeEnd = coerce BS.takeEnd

takeWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhileEnd = coerce BS.takeWhileEnd

takeWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhile = coerce BS.takeWhile

drop :: Int -> ShortByteString -> ShortByteString
drop = coerce BS.drop

dropEnd :: Int -> ShortByteString -> ShortByteString
dropEnd = coerce BS.dropEnd

dropWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhile = coerce BS.dropWhile

dropWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd = coerce BS.dropWhileEnd

breakEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd = coerce BS.breakEnd

break :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break = coerce BS.break

span :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
span = coerce BS.span

spanEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd = coerce BS.spanEnd

splitAt :: Int -> ShortByteString -> (ShortByteString, ShortByteString)
splitAt = coerce BS.splitAt

split :: Word8 -> ShortByteString -> [ShortByteString]
split = coerce BS.split

splitWith :: (Word8 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith = coerce BS.splitWith

stripSuffix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripSuffix = coerce BS.stripSuffix

stripPrefix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripPrefix = coerce BS.stripPrefix

isInfixOf :: ShortByteString -> ShortByteString -> Bool
isInfixOf = coerce BS.isInfixOf

isPrefixOf :: ShortByteString -> ShortByteString -> Bool
isPrefixOf = coerce BS.isPrefixOf

isSuffixOf :: ShortByteString -> ShortByteString -> Bool
isSuffixOf = coerce BS.isSuffixOf

breakSubstring :: ShortByteString -> ShortByteString -> (ShortByteString, ShortByteString)
breakSubstring = coerce BS.breakSubstring

elem :: Word8 -> ShortByteString -> Bool
elem = coerce BS.elem

find :: (Word8 -> Bool) -> ShortByteString -> Maybe Word8
find = coerce BS.find

filter :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
filter = coerce BS.filter

partition :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
partition = coerce BS.partition

index :: {- HasCallStack => -} ShortByteString -> Int -> Word8
index = coerce BS.index

indexMaybe :: ShortByteString -> Int -> Maybe Word8
indexMaybe = coerce BS.indexMaybe

(!?) :: ShortByteString -> Int -> Maybe Word8
(!?) = coerce (BS.!?)

elemIndex :: Word8 -> ShortByteString -> Maybe Int
elemIndex = coerce BS.elemIndex

elemIndices :: Word8 -> ShortByteString -> [Int]
elemIndices = coerce BS.elemIndices

count :: Word8 -> ShortByteString -> Int
count = coerce BS.count

findIndex :: (Word8 -> Bool) -> ShortByteString -> Maybe Int
findIndex = coerce BS.findIndex

findIndices :: (Word8 -> Bool) -> ShortByteString -> [Int]
findIndices = coerce BS.findIndices

unsafeIndex :: ShortByteString -> Int -> Word8
unsafeIndex = coerce BS.Unsafe.unsafeIndex

{- XXX
createFromPtr :: Ptr a -> Int -> IO ShortByteString
createFromPtr p = coerce . BS.createFromPtr p

copyToPtr :: ShortByteString -> Int -> Ptr a -> Int -> IO ()
copyToPtr = BS.copyToPtr . coerce
-}

isValidUtf8 :: ShortByteString -> Bool
isValidUtf8 = coerce BS.isValidUtf8

packCString :: CString -> IO ShortByteString
packCString = coerce BS.packCString

packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen = coerce BS.packCStringLen

useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString = BS.useAsCString . coerce

useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen = BS.useAsCStringLen . coerce

-- private

-- truncates wide characters
packChars :: [Char] -> ShortByteString
packChars = toShort . BS.pack . fmap (toEnum . fromEnum)

unpackChars :: ShortByteString -> [Char]
unpackChars = fmap  (toEnum . fromEnum) . BS.unpack . fromShort
