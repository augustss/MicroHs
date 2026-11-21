module Dynamic(main) where
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "dynamic.h &sub3"  p_sub3  :: FunPtr (CInt -> CInt -> CInt -> IO CInt)
foreign import ccall "dynamic.h &neg8"  p_neg8  :: FunPtr (Int8 -> Int8)
foreign import ccall "dynamic.h &inc16" p_inc16 :: FunPtr (Word16 -> IO Word16)
foreign import ccall "dynamic.h &halve" p_halve :: FunPtr (CFloat -> CFloat)
foreign import ccall "dynamic.h &scale" p_scale :: FunPtr (CDouble -> CLong -> IO CDouble)
foreign import ccall "dynamic.h &big"   p_big   :: FunPtr (IO CLLong)
foreign import ccall "dynamic.h &store" p_store :: FunPtr (Ptr CInt -> CInt -> IO ())
foreign import ccall "dynamic.h &pick"  p_pick  :: FunPtr (CInt -> IO (FunPtr (CInt -> CInt -> CInt -> IO CInt)))

foreign import ccall "dynamic" sub3  :: FunPtr (CInt -> CInt -> CInt -> IO CInt) -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "dynamic" neg8  :: FunPtr (Int8 -> Int8) -> Int8 -> Int8
foreign import ccall "dynamic" inc16 :: FunPtr (Word16 -> IO Word16) -> Word16 -> IO Word16
foreign import ccall "dynamic" halve :: FunPtr (CFloat -> CFloat) -> CFloat -> CFloat
foreign import ccall "dynamic" scale :: FunPtr (CDouble -> CLong -> IO CDouble) -> CDouble -> CLong -> IO CDouble
foreign import ccall "dynamic" big   :: FunPtr (IO CLLong) -> IO CLLong
foreign import ccall "dynamic" store :: FunPtr (Ptr CInt -> CInt -> IO ()) -> Ptr CInt -> CInt -> IO ()
foreign import ccall "dynamic" pick  :: FunPtr (CInt -> IO (FunPtr (CInt -> CInt -> CInt -> IO CInt))) -> CInt -> IO (FunPtr (CInt -> CInt -> CInt -> IO CInt))

main :: IO ()
main = do
  print =<< sub3 p_sub3 2 5 10
  print (neg8 p_neg8 5, neg8 p_neg8 (-128))
  print =<< inc16 p_inc16 65535
  print (halve p_halve 3)
  print =<< scale p_scale 1.5 (-4)
  print =<< big p_big
  alloca $ \ p -> do
    store p_store p (-42)
    print =<< peek p
  f <- pick p_pick 1
  print (f == castFunPtr p_sub3)
  print =<< sub3 f 100 (sum [1..10]) 1
