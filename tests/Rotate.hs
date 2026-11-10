module Rotate where

import Data.Bits
import Data.Int
import Data.Word

counts :: [Int]
counts = [-9, -1, 0, 1, 3, 7, 8, 9, 31, 32, 33, 63, 64, 65]

rots :: (FiniteBits a, Num a, Show a) => a -> IO ()
rots x = do
  print [ rotate  x i | i <- counts ]
  print [ rotateL x i | i <- counts, i >= 0 ]
  print [ rotateR x i | i <- counts, i >= 0 ]

main :: IO ()
main = do
  print $ rotateR (1 :: Word32) 6
  rots (0x81 :: Word8)
  rots (0x8001 :: Word16)
  rots (0x80000001 :: Word32)
  rots (0x8000000000000001 :: Word64)
  rots (-127 :: Int8)
  rots (-32767 :: Int16)
  rots (-2147483647 :: Int32)
  rots (-9223372036854775807 :: Int64)
  rots (42 :: Int8)
  rots (minBound :: Int32)
  -- Int and Word depend on the word size
  let w = finiteBitSize (0 :: Int)
  print $ rotateR (1 :: Word) 1 == bit (w - 1)
  print $ rotateL (bit (w - 1) :: Word) 1 == 1
  print $ rotateR (1 :: Int) 1 == minBound
  print $ rotateL (minBound :: Int) 1 == 1
  print $ rotate (-2 :: Int) w == -2
  print $ rotateR (-2 :: Int) 1 == maxBound
