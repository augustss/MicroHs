-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(Word, Word8, Word16, Word32, Word64) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits
import Data.Bool_Type
import Data.Bounded
import Data.Char
import Data.Enum
import Data.Eq
import Data.Function
import Data.Int()  -- instances only
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Real
import Numeric.Show
import Text.Show

instance Num Word where
  (+)  = primWordAdd
  (-)  = primWordSub
  (*)  = primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger x = primIntToWord (_integerToInt x)

instance Integral Word where
  quot = primWordQuot
  rem  = primWordRem
  toInteger = _wordToInteger

instance Bounded Word where
  minBound = 0::Word
  maxBound = primWordInv (0::Word)

instance Real Word where
  toRational i = _integerToRational (_wordToInteger i)

instance Show Word where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word where
  readsPrec = readIntegral
-}

--------------------------------

instance Enum Word where
  succ x = x + 1
  pred x = x - 1
  toEnum = primIntToWord
  fromEnum = primWordToInt
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

--------------------------------

instance Eq Word where
  (==) = primWordEQ
  (/=) = primWordNE

instance Ord Word where
  compare = primWordCompare
  (<)  = primWordLT
  (<=) = primWordLE
  (>)  = primWordGT
  (>=) = primWordGE

--------------------------------

instance Bits Word where
  (.&.) = primWordAnd
  (.|.) = primWordOr
  xor   = primWordXor
  complement = primWordInv
  shiftL = primWordShl
  shiftR = primWordShr
  bitSizeMaybe _ = Just _wordSize
  bitSize _ = _wordSize
  bit n = primWordShl 1 n
  zeroBits = 0


instance FiniteBits Word where
  finiteBitSize _ = _wordSize

--------------------------------------------------------------------------------
----    Word8

newtype Word8 = W8 Word
unW8 :: Word8 -> Word
unW8 (W8 x) = x

w8 :: Word -> Word8
w8 w = W8 (w .&. 0xff)

bin8 :: (Word -> Word -> Word) -> (Word8 -> Word8 -> Word8)
bin8 op (W8 x) (W8 y) = w8 (x `op` y)

bini8 :: (Word -> Int -> Word) -> (Word8 -> Int -> Word8)
bini8 op (W8 x) y = w8 (x `op` y)

cmp8 :: (Word -> Word -> a) -> (Word8 -> Word8 -> a)
cmp8 op (W8 x) (W8 y) = x `op` y

una8 :: (Word -> Word) -> (Word8 -> Word8)
una8 op (W8 x) = w8 (op x)

instance Num Word8 where
  (+)  = bin8 primWordAdd
  (-)  = bin8 primWordSub
  (*)  = bin8 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w8 (primIntToWord (_integerToInt i))

instance Integral Word8 where
  quot = bin8 primWordQuot
  rem  = bin8 primWordRem
  toInteger = _wordToInteger . unW8

instance Bounded Word8 where
  minBound = W8 0
  maxBound = W8 0xff

instance Real Word8 where
  toRational = _integerToRational . _wordToInteger . unW8

instance Show Word8 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word8 where
  readsPrec = readIntegral
-}

instance Enum Word8 where
  succ x = x + 1
  pred x = x - 1
  toEnum = w8 . primIntToWord
  fromEnum = primWordToInt . unW8
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

instance Eq Word8 where
  (==) = cmp8 primWordEQ
  (/=) = cmp8 primWordNE

instance Ord Word8 where
  compare = cmp8 primWordCompare
  (<)  = cmp8 primWordLT
  (<=) = cmp8 primWordLE
  (>)  = cmp8 primWordGT
  (>=) = cmp8 primWordGE

instance Bits Word8 where
  (.&.) = bin8 primWordAnd
  (.|.) = bin8 primWordOr
  xor   = bin8 primWordXor
  complement = una8 primWordInv
  shiftL = bini8 primWordShl
  shiftR = bini8 primWordShr
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  bit n = w8 (primWordShl 1 n)
  zeroBits = 0

instance FiniteBits Word8 where
  finiteBitSize _ = 8

--------------------------------------------------------------------------------
----    Word16

newtype Word16 = W16 Word
unW16 :: Word16 -> Word
unW16 (W16 x) = x

w16 :: Word -> Word16
w16 w = W16 (w .&. 0xffff)

bin16 :: (Word -> Word -> Word) -> (Word16 -> Word16 -> Word16)
bin16 op (W16 x) (W16 y) = w16 (x `op` y)

bini16 :: (Word -> Int -> Word) -> (Word16 -> Int -> Word16)
bini16 op (W16 x) y = w16 (x `op` y)

cmp16 :: (Word -> Word -> a) -> (Word16 -> Word16 -> a)
cmp16 op (W16 x) (W16 y) = x `op` y

una16 :: (Word -> Word) -> (Word16 -> Word16)
una16 op (W16 x) = w16 (op x)

instance Num Word16 where
  (+)  = bin16 primWordAdd
  (-)  = bin16 primWordSub
  (*)  = bin16 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w16 (primIntToWord (_integerToInt i))

instance Integral Word16 where
  quot = bin16 primWordQuot
  rem  = bin16 primWordRem
  toInteger = _wordToInteger . unW16

instance Bounded Word16 where
  minBound = W16 0
  maxBound = W16 0xffff

instance Real Word16 where
  toRational = _integerToRational . _wordToInteger . unW16

instance Show Word16 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word16 where
  readsPrec = readIntegral
-}

instance Enum Word16 where
  succ x = x + 1
  pred x = x - 1
  toEnum = w16 . primIntToWord
  fromEnum = primWordToInt . unW16
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

instance Eq Word16 where
  (==) = cmp16 primWordEQ
  (/=) = cmp16 primWordNE

instance Ord Word16 where
  compare = cmp16 primWordCompare
  (<)  = cmp16 primWordLT
  (<=) = cmp16 primWordLE
  (>)  = cmp16 primWordGT
  (>=) = cmp16 primWordGE

instance Bits Word16 where
  (.&.) = bin16 primWordAnd
  (.|.) = bin16 primWordOr
  xor   = bin16 primWordXor
  complement = una16 primWordInv
  shiftL = bini16 primWordShl
  shiftR = bini16 primWordShr
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  bit n = w16 (primWordShl 1 n)
  zeroBits = 0

instance FiniteBits Word16 where
  finiteBitSize _ = 16

--------------------------------------------------------------------------------
----    Word32

newtype Word32 = W32 Word
unW32 :: Word32 -> Word
unW32 (W32 x) = x

w32 :: Word -> Word32
w32 w = if _wordSize == 32 then W32 w else W32 (w .&. 0xffffffff)

bin32 :: (Word -> Word -> Word) -> (Word32 -> Word32 -> Word32)
bin32 op (W32 x) (W32 y) = w32 (x `op` y)

bini32 :: (Word -> Int -> Word) -> (Word32 -> Int -> Word32)
bini32 op (W32 x) y = w32 (x `op` y)

cmp32 :: (Word -> Word -> a) -> (Word32 -> Word32 -> a)
cmp32 op (W32 x) (W32 y) = x `op` y

una32 :: (Word -> Word) -> (Word32 -> Word32)
una32 op (W32 x) = w32 (op x)

instance Num Word32 where
  (+)  = bin32 primWordAdd
  (-)  = bin32 primWordSub
  (*)  = bin32 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w32 (primIntToWord (_integerToInt i))

instance Integral Word32 where
  quot = bin32 primWordQuot
  rem  = bin32 primWordRem
  toInteger = _wordToInteger . unW32

instance Bounded Word32 where
  minBound = W32 0
  maxBound = W32 0xffffffff

instance Real Word32 where
  toRational = _integerToRational . _wordToInteger . unW32

instance Show Word32 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word32 where
  readsPrec = readIntegral
-}

instance Enum Word32 where
  succ x = x + 1
  pred x = x - 1
  toEnum = w32 . primIntToWord
  fromEnum = primWordToInt . unW32
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

instance Eq Word32 where
  (==) = cmp32 primWordEQ
  (/=) = cmp32 primWordNE

instance Ord Word32 where
  compare = cmp32 primWordCompare
  (<)  = cmp32 primWordLT
  (<=) = cmp32 primWordLE
  (>)  = cmp32 primWordGT
  (>=) = cmp32 primWordGE

instance Bits Word32 where
  (.&.) = bin32 primWordAnd
  (.|.) = bin32 primWordOr
  xor   = bin32 primWordXor
  complement = una32 primWordInv
  shiftL = bini32 primWordShl
  shiftR = bini32 primWordShr
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  bit n = w32 (primWordShl 1 n)
  zeroBits = 0

instance FiniteBits Word32 where
  finiteBitSize _ = 32

--------------------------------------------------------------------------------
----    Word64

newtype Word64 = W64 Word
unW64 :: Word64 -> Word
unW64 (W64 x) = x

w64 :: Word -> Word64
w64 w = if _wordSize == 64 then W64 w else error "No Word64"

bin64 :: (Word -> Word -> Word) -> (Word64 -> Word64 -> Word64)
bin64 op (W64 x) (W64 y) = w64 (x `op` y)

bini64 :: (Word -> Int -> Word) -> (Word64 -> Int -> Word64)
bini64 op (W64 x) y = w64 (x `op` y)

cmp64 :: (Word -> Word -> a) -> (Word64 -> Word64 -> a)
cmp64 op (W64 x) (W64 y) = x `op` y

una64 :: (Word -> Word) -> (Word64 -> Word64)
una64 op (W64 x) = w64 (op x)

instance Num Word64 where
  (+)  = bin64 primWordAdd
  (-)  = bin64 primWordSub
  (*)  = bin64 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w64 (primIntToWord (_integerToInt i))

instance Integral Word64 where
  quot = bin64 primWordQuot
  rem  = bin64 primWordRem
  toInteger = _wordToInteger . unW64

instance Bounded Word64 where
  minBound = W64 0
  maxBound = W64 0xffffffffffffffff

instance Real Word64 where
  toRational = _integerToRational . _wordToInteger . unW64

instance Show Word64 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word64 where
  readsPrec = readIntegral
-}

instance Enum Word64 where
  succ x = x + 1
  pred x = x - 1
  toEnum = w64 . primIntToWord
  fromEnum = primWordToInt . unW64
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

instance Eq Word64 where
  (==) = cmp64 primWordEQ
  (/=) = cmp64 primWordNE

instance Ord Word64 where
  compare = cmp64 primWordCompare
  (<)  = cmp64 primWordLT
  (<=) = cmp64 primWordLE
  (>)  = cmp64 primWordGT
  (>=) = cmp64 primWordGE

instance Bits Word64 where
  (.&.) = bin64 primWordAnd
  (.|.) = bin64 primWordOr
  xor   = bin64 primWordXor
  complement = una64 primWordInv
  shiftL = bini64 primWordShl
  shiftR = bini64 primWordShr
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  bit n = w64 (primWordShl 1 n)
  zeroBits = 0

instance FiniteBits Word64 where
  finiteBitSize _ = 64
