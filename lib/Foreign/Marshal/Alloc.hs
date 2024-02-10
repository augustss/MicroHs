module Foreign.Marshal.Alloc(
  malloc, calloc, alloca,
  free,
  mallocBytes, callocBytes,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error(undefined)
import Foreign.Storable

foreign import ccall "free" c_free :: forall a . Ptr a -> IO ()

free :: forall a . Ptr a -> IO ()
free = c_free

foreign import ccall "malloc" c_malloc :: forall a . Int -> IO (Ptr a)

mallocBytes :: forall a . Int -> IO (Ptr a)
mallocBytes = c_malloc

foreign import ccall "calloc" c_calloc :: forall a . Int -> Int -> IO (Ptr a)

callocBytes :: forall a . Int -> IO (Ptr a)
callocBytes = c_calloc (1::Int)

malloc :: forall a . Storable a => IO (Ptr a)
malloc  = mallocBytes (sizeOf (undefined :: a))

calloc :: forall a . Storable a => IO (Ptr a)
calloc = callocBytes (sizeOf (undefined :: a))

alloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca io =
  mallocBytes (sizeOf (undefined :: a)) `primBind` (\ p ->
  io p `primBind` (\ b ->
  free p `primThen`
  primReturn b))
