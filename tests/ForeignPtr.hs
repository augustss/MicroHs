module ForeignPtr(main) where
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

gc :: IO ()
gc = primitive "IO.gc"

add :: Ptr a -> Int -> Ptr a
add = plusPtr

main :: IO ()
main = do
  fp <- mallocForeignPtrArray 2
  withForeignPtr fp $ \ p -> do
    poke p (42::Int)
    poke (add p 8) (88::Int)
  withForeignPtr fp $ \ p -> do
    gc
    peek p >>= print
    peek (add p 8) >>= print
  let fp1 :: ForeignPtr Int
      fp1 = plusForeignPtr fp 8
  withForeignPtr fp1 $ \ p -> do
    peek p >>= print
  gc
