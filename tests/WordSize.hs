module WordSize(main) where
-- The word size is a constant of the runtime (the "wordsize" primitive);
-- everything in Data.Bits that depends on it.
import Data.Bits

main :: IO ()
main = do
  print (finiteBitSize (0 :: Int), finiteBitSize (0 :: Word))
  print (bit 3 :: Int, testBit (5 :: Int) 2, testBit (5 :: Int) 1)
  print ((1 :: Int) `shiftL` (finiteBitSize (0 :: Int)), (-8 :: Int) `shiftR` 1, (5 :: Int) `shiftR` 100)
  print (countLeadingZeros (1 :: Int), countTrailingZeros (8 :: Word))
