module Integer(main) where

import Data.Bits
import Data.Int
import Data.Word
import System.IO.Serialize
import System.IO.StringHandle

main :: IO ()
main = do
  print $ (1000::Integer) == 1000
  print $ ((10::Integer)^(100::Int)) /= 0
  print $ show (product [1..100::Integer]) == "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
  let i = 1234567890123456789012345678901234567890 :: Integer
  print i
  s <- handleWriteToString (\ h -> hSerialize h i)
  h <- stringToHandle s
  i' <- hDeserialize h
  print $ i == i'

  -- Integer conversion
  let
    iMinI64 = -9223372036854775808
    iMinI32 = -2147483648
    iNegOne = -1
    iZero = 0
    iOne = 1
    iMaxI32 = 2147483647
    iMaxU32 = 4294967295
    iMaxI64 = 9223372036854775807
    iMaxU64 = 18446744073709551615
  putStrLn "fromInteger"
  print $ (fromInteger iMinI64 :: Int64) == (minBound :: Int64)
  print $ (fromInteger iMinI64 :: Int32) == 0
  print $ (fromInteger iMinI64 :: Word64) == 1 `shiftL` 63
  print $ (fromInteger iMinI64 :: Word32) == 0
  print $ (fromInteger iMinI32 :: Int64) == complement ((1 `shiftL` 31) - 1)
  print $ (fromInteger iMinI32 :: Int32) == (minBound :: Int32)
  print $ (fromInteger iMinI32 :: Word64) == complement ((1 `shiftL` 31) - 1)
  print $ (fromInteger iMinI32 :: Word32) == 1 `shiftL` 31
  print $ (fromInteger iNegOne :: Int64) == -1
  print $ (fromInteger iNegOne :: Int32) == -1
  print $ (fromInteger iNegOne :: Word64) == (maxBound :: Word64)
  print $ (fromInteger iNegOne :: Word32) == (maxBound :: Word32)
  print $ (fromInteger iZero :: Int64) == 0
  print $ (fromInteger iZero :: Int32) == 0
  print $ (fromInteger iZero :: Word64) == 0
  print $ (fromInteger iZero :: Word32) == 0
  print $ (fromInteger iOne :: Int64) == 1
  print $ (fromInteger iOne :: Int32) == 1
  print $ (fromInteger iOne :: Word64) == 1
  print $ (fromInteger iOne :: Word32) == 1
  print $ (fromInteger iMaxI32 :: Int64) == (1 `shiftL` 31) - 1
  print $ (fromInteger iMaxI32 :: Int32) == (maxBound :: Int32)
  print $ (fromInteger iMaxI32 :: Word64) == (1 `shiftL` 31) - 1
  print $ (fromInteger iMaxI32 :: Word32) == (1 `shiftL` 31) - 1
  print $ (fromInteger iMaxU32 :: Int64) == (1 `shiftL` 32) - 1
  print $ (fromInteger iMaxU32 :: Int32) == -1
  print $ (fromInteger iMaxU32 :: Word64) == (1 `shiftL` 32) - 1
  print $ (fromInteger iMaxU32 :: Word32) == (maxBound :: Word32)
  print $ (fromInteger iMaxI64 :: Int64) == (maxBound :: Int64)
  print $ (fromInteger iMaxI64 :: Int32) == -1
  print $ (fromInteger iMaxI64 :: Word64) == (1 `shiftL` 63) - 1
  print $ (fromInteger iMaxI64 :: Word32) == (maxBound :: Word32)
  print $ (fromInteger iMaxU64 :: Int64) == -1
  print $ (fromInteger iMaxU64 :: Int32) == -1
  print $ (fromInteger iMaxU64 :: Word64) == (maxBound :: Word64)
  print $ (fromInteger iMaxU64 :: Word32) == (maxBound :: Word32)
  putStrLn "toInteger"
  print $ toInteger (minBound :: Int64) == iMinI64
  print $ toInteger (minBound :: Int32) == iMinI32
  print $ toInteger (-1 :: Int) == iNegOne
  print $ toInteger (-1 :: Int64) == iNegOne
  print $ toInteger (0 :: Int) == iZero
  print $ toInteger (0 :: Int64) == iZero
  print $ toInteger (0 :: Word) == iZero
  print $ toInteger (0 :: Word64) == iZero
  print $ toInteger (1 :: Int) == iOne
  print $ toInteger (1 :: Int64) == iOne
  print $ toInteger (1 :: Word) == iOne
  print $ toInteger (1 :: Word64) == iOne
  print $ toInteger (maxBound :: Int64) == iMaxI64
  print $ toInteger (maxBound :: Int32) == iMaxI32
  print $ toInteger (maxBound :: Word64) == iMaxU64
  print $ toInteger (maxBound :: Word32) == iMaxU32
