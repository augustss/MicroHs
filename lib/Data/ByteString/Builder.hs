module Data.ByteString.Builder
    ( Builder
    , toLazyByteString
    , hPutBuilder
    , writeFile

    , byteString
    , lazyByteString
    , shortByteString
    , int8
    , word8

    , int16BE
    , int32BE
    , int64BE

    , word16BE
    , word32BE
    , word64BE

    , floatBE
    , doubleBE

    , int16LE
    , int32LE
    , int64LE

    , word16LE
    , word32LE
    , word64LE

    , floatLE
    , doubleLE

    , char7
    , string7

    , char8
    , string8

    , charUtf8
    , stringUtf8

    , module Data.ByteString.Builder.ASCII
    , module Data.ByteString.Builder.RealFloat
    ) where

import qualified Prelude ()
import MiniPrelude

import Data.Bits
import Data.ByteString.Builder.Internal
import Data.ByteString.Lazy as L
import Data.ByteString.Short.Internal (ShortByteString, fromShort)
import Data.Char (ord)
import qualified Data.List as List
import Data.Int
import Data.Word
import Data.Word.Word8 (intToWord8)
import Numeric.Show (showHex)
import Primitives
import System.IO.Internal
import Unsafe.Coerce (unsafeCoerce)

import Data.ByteString.Builder.ASCII
import Data.ByteString.Builder.RealFloat

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h = hPut h . toLazyByteString

writeFile :: FilePath -> Builder -> IO ()
writeFile f = L.writeFile f . toLazyByteString

shortByteString :: ShortByteString -> Builder
shortByteString = byteString . fromShort

int8 :: Int8 -> Builder
int8 = word8 . fromIntegral

word8 :: Word8 -> Builder
word8 w = lazyByteString $ pack [w]

int16BE :: Int16 -> Builder
int16BE = word16BE . fromIntegral

int32BE :: Int32 -> Builder
int32BE = word32BE . fromIntegral

int64BE :: Int64 -> Builder
int64BE = word64BE . fromIntegral

word16BE :: Word16 -> Builder
word16BE w = lazyByteString $ pack
    [ fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

word32BE :: Word32 -> Builder
word32BE w = lazyByteString $ pack
    [ fromIntegral ((w `shiftR` 24) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

word64BE :: Word64 -> Builder
word64BE w = lazyByteString $ pack
    [ fromIntegral ((w `shiftR` 56) .&. 0xff)
    , fromIntegral ((w `shiftR` 48) .&. 0xff)
    , fromIntegral ((w `shiftR` 40) .&. 0xff)
    , fromIntegral ((w `shiftR` 32) .&. 0xff)
    , fromIntegral ((w `shiftR` 24) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

floatBE :: Float -> Builder
floatBE = word32BE . unsafeCoerce . primWordFromFloatRaw

doubleBE :: Double -> Builder
doubleBE = word64BE . primWord64FromDoubleRaw

int16LE :: Int16 -> Builder
int16LE = word16LE . fromIntegral

int32LE :: Int32 -> Builder
int32LE = word32LE . fromIntegral

int64LE :: Int64 -> Builder
int64LE = word64LE . fromIntegral

word16LE :: Word16 -> Builder
word16LE w = lazyByteString $ pack
    [ fromIntegral (w .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    ]

word32LE :: Word32 -> Builder
word32LE w = lazyByteString $ pack
    [ fromIntegral (w .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 24) .&. 0xff)
    ]

word64LE :: Word64 -> Builder
word64LE w = lazyByteString $ pack
    [ fromIntegral (w .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 24) .&. 0xff)
    , fromIntegral ((w `shiftR` 32) .&. 0xff)
    , fromIntegral ((w `shiftR` 40) .&. 0xff)
    , fromIntegral ((w `shiftR` 48) .&. 0xff)
    , fromIntegral ((w `shiftR` 56) .&. 0xff)
    ]

floatLE :: Float -> Builder
floatLE = word32LE . unsafeCoerce . primWordFromFloatRaw

doubleLE :: Double -> Builder
doubleLE = word64LE . primWord64FromDoubleRaw

char7 :: Char -> Builder
char7 c = word8 (intToWord8 (ord c .&. 0x7f))

string7 :: String -> Builder
string7 = lazyByteString . pack . List.map (\c -> intToWord8 (ord c .&. 0x7f))

char8 :: Char -> Builder
char8 c = word8 (intToWord8 (ord c .&. 0xff))

string8 :: String -> Builder
string8 = lazyByteString . pack . List.map (\c -> intToWord8 (ord c .&. 0xff))

charUtf8 :: Char -> Builder
charUtf8 c = stringUtf8 [c]
