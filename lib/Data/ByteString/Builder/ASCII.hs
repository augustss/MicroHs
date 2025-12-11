module Data.ByteString.Builder.ASCII where

import qualified Prelude ()
import MiniPrelude

import Data.Bits
import Data.ByteString.Builder.Internal
import Data.ByteString.Internal (StrictByteString)
import qualified Data.ByteString.Internal as S
import Data.ByteString.Lazy.Internal (LazyByteString, packChars, unpackBytes)
import Data.Char
import Data.Int
import Data.Integer
import Data.Word
import Data.Word.Word8 (word8ToInt)
import Numeric.Show (showHex)
import Primitives
import Unsafe.Coerce (unsafeCoerce)

int8Dec :: Int8 -> Builder
int8Dec = stringUtf8 . show

int16Dec :: Int16 -> Builder
int16Dec = stringUtf8 . show

int32Dec :: Int32 -> Builder
int32Dec = stringUtf8 . show

int64Dec :: Int64 -> Builder
int64Dec = stringUtf8 . show

intDec :: Int -> Builder
intDec = stringUtf8 . show

integerDec :: Integer -> Builder
integerDec = stringUtf8 . show

word8Dec :: Word8 -> Builder
word8Dec = stringUtf8 . show

word16Dec :: Word16 -> Builder
word16Dec = stringUtf8 . show

word32Dec :: Word32 -> Builder
word32Dec = stringUtf8 . show

word64Dec :: Word64 -> Builder
word64Dec = stringUtf8 . show

wordDec :: Word -> Builder
wordDec = stringUtf8 . show

word8Hex :: Word8 -> Builder
word8Hex w = stringUtf8 (showHex w "")

word16Hex :: Word16 -> Builder
word16Hex w = stringUtf8 (showHex w "")

word32Hex :: Word32 -> Builder
word32Hex w = stringUtf8 (showHex w "")

word64Hex :: Word64 -> Builder
word64Hex w = stringUtf8 (showHex w "")

wordHex :: Word -> Builder
wordHex w = stringUtf8 (showHex w "")

int8HexFixed :: Int8 -> Builder
int8HexFixed = word8HexFixed . fromIntegral

int16HexFixed :: Int16 -> Builder
int16HexFixed = word16HexFixed . fromIntegral

int32HexFixed :: Int32 -> Builder
int32HexFixed = word32HexFixed . fromIntegral

int64HexFixed :: Int64 -> Builder
int64HexFixed = word64HexFixed . fromIntegral

word8HexFixed :: Word8 -> Builder
word8HexFixed w = stringUtf8
    [ intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 4) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce (w .&. 0xf)
    ]

word16HexFixed :: Word16 -> Builder
word16HexFixed w = stringUtf8
    [ intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 12) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 8) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 4) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce (w .&. 0xf)
    ]

word32HexFixed :: Word32 -> Builder
word32HexFixed w = stringUtf8
    [ intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 28) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 24) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 20) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 16) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 12) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 8) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce ((w `shiftR` 4) .&. 0xf)
    , intToDigit . primWordToInt $ unsafeCoerce (w .&. 0xf)
    ]

word64HexFixed :: Word64 -> Builder
word64HexFixed w = stringUtf8
    [ intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 60) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 56) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 52) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 48) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 44) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 40) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 36) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 32) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 28) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 24) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 20) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 16) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 12) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 8) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord ((w `shiftR` 4) .&. 0xf)
    , intToDigit . primWordToInt $ primWord64ToWord (w .&. 0xf)
    ]

floatHexFixed :: Float -> Builder
floatHexFixed = word32HexFixed . unsafeCoerce . primWordFromFloatRaw

doubleHexFixed :: Double -> Builder
doubleHexFixed = word64HexFixed . primWord64FromDoubleRaw

byteStringHex :: StrictByteString -> Builder
byteStringHex = stringUtf8 . concatMap (\w -> [intToDigit $ word8ToInt ((w `shiftR` 4) .&. 0xf), intToDigit $ word8ToInt (w .&. 0xf)]) . S.unpack

lazyByteStringHex :: LazyByteString -> Builder
lazyByteStringHex = stringUtf8 . concatMap (\w -> [intToDigit $ word8ToInt ((w `shiftR` 4) .&. 0xf), intToDigit $ word8ToInt (w .&. 0xf)]) . unpackBytes
