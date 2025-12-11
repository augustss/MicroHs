module Builder where

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as S
import Data.Int
import Data.Word

builder :: Builder
builder = mempty
    <> byteString (B.pack [32..64])
    <> lazyByteString (L.pack [65..126])
    <> shortByteString (S.pack [0xff, 0xfe])
    <> int8 0xf0
    <> word8 0xf0
    <> int16BE 0xf063
    <> int32BE 0xf063481a
    <> int64BE 0xf063481acbd25901
    <> word16BE 0x63f0
    <> word32BE 0x1a4863f0
    <> word64BE 0x0159d2cb1a4863f0
    <> floatBE (sqrt 2)
    <> doubleBE (sqrt 2)
    <> int16LE 0xf063
    <> int32LE 0xf063481a
    <> int64LE 0xf063481acbd25901
    <> word16LE 0x63f0
    <> word32LE 0x1a4863f0
    <> word64LE 0x0159d2cb1a4863f0
    <> floatLE 1.0
    <> doubleLE 1.0
    <> char7 '$'
    <> string7 "hello"
    <> char8 'µ'
    <> stringUtf8 "Ölrückstoßabdämpfung"
    <> charUtf8 '€'
    <> int8Dec minBound
    <> int16Dec minBound
    <> int32Dec minBound
    <> int64Dec minBound
    <> intDec (-42)
    <> integerDec (2 ^ 127 - 1)
    <> word8Dec maxBound
    <> word16Dec maxBound
    <> word32Dec maxBound
    <> word64Dec maxBound
    <> wordDec 42
    <> word8Hex maxBound
    <> word16Hex maxBound
    <> word32Hex maxBound
    <> word64Hex maxBound
    <> wordHex 42
    <> int8HexFixed (-123)
    <> int16HexFixed (-1234)
    <> int32HexFixed (-12345)
    <> int64HexFixed (-123456)
    <> word8HexFixed 123
    <> word16HexFixed 1234
    <> word32HexFixed 12345
    <> word64HexFixed 123456
    <> floatHexFixed (sqrt 2)
    <> doubleHexFixed (sqrt 2)
    <> byteStringHex (B.pack [32..64])
    <> lazyByteStringHex (L.pack [65..126])
    <> floatDec (sqrt 2)
    <> doubleDec (sqrt 2)

main :: IO ()
main = print $ toLazyByteString builder
