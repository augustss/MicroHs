module Storable(main) where
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

pokePeek :: (Storable a, Bounded a) => IO a
pokePeek = alloca (\p -> poke p maxBound >> peek p)

main :: IO ()
main = do
  let w1 = 0x01020304 :: Word
  p1 <- new w1
  wp1 <- peek p1
  print $ wp1 == w1

  let w2 = [1,2,3,4,5::Word]
  p2 <- newArray0 0 w2
  wp2 <- peekArray0 0 p2
  print $ wp2 == w2

  moveBytes (p2 `plusPtr` sizeOf w1) p2 (4 * sizeOf w1)
  poke p2 w1
  wp3 <- peekArray0 0 p2
  print $ wp3 == [w1,1,2,3,4]

{- Relies on endianess
  let p3 = castPtr p1 :: Ptr Word8
  b1 <- peek p3
  b2 <- peek (p3 `plusPtr` 1)
  print [b1, b2]
-}

  pokePeek @Word8  >>= print
  pokePeek @Word16 >>= print
  pokePeek @Word32 >>= print
  pokePeek @Word64 >>= print
  pokePeek @Int8   >>= print
  pokePeek @Int16  >>= print
  pokePeek @Int32  >>= print
  pokePeek @Int64  >>= print
