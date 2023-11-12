module Foreign.Marshal.Alloc(
  free,
  mallocBytes,
  ) where
import Primitives

foreign import ccall "free" c_free :: forall a . Ptr a -> IO ()

free :: forall a . Ptr a -> IO ()
free = c_free

foreign import ccall "malloc" c_malloc :: forall a . Int -> IO (Ptr a)

mallocBytes :: forall a . Int -> IO (Ptr a)
mallocBytes = c_malloc
