module Storable(main) where
import Prelude
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

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
