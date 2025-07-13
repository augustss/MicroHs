module Foreign.Marshal.Alloc(
  malloc, calloc, realloc, alloca,
  free, finalizerFree,
  mallocBytes, callocBytes, reallocBytes, allocaBytes,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error(undefined)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr_Type
import Foreign.Storable

foreign import ccall "free" c_free :: Ptr () -> IO ()

free :: forall a . Ptr a -> IO ()
free p = c_free (castPtr p)

foreign import ccall "&free" c_freefun :: IO (FinalizerPtr ())

finalizerFree :: FinalizerPtr a
finalizerFree = primUnsafeCoerce (primPerformIO c_freefun)

foreign import ccall "malloc" c_malloc :: CSize -> IO (Ptr ())

mallocBytes :: forall a . Int -> IO (Ptr a)
mallocBytes n = c_malloc (intToCSize n) `primBind` \ p -> primReturn (castPtr p)

foreign import ccall "calloc" c_calloc :: CSize -> CSize -> IO (Ptr ())

callocBytes :: forall a . Int -> IO (Ptr a)
callocBytes n = c_calloc (intToCSize (1::Int)) (intToCSize n) `primBind` \p -> primReturn (castPtr p)

foreign import ccall "realloc" c_realloc :: Ptr () -> CSize -> IO (Ptr ())

reallocBytes :: Ptr a -> Int -> IO (Ptr a)
reallocBytes p n = c_realloc (castPtr p) (intToCSize n) `primBind` \q -> primReturn (castPtr q)

malloc :: forall a . Storable a => IO (Ptr a)
malloc  = mallocBytes (sizeOf (undefined :: a))

calloc :: forall a . Storable a => IO (Ptr a)
calloc = callocBytes (sizeOf (undefined :: a))

realloc :: forall a b . Storable b => Ptr a -> IO (Ptr b)
realloc p = reallocBytes (castPtr p) (sizeOf (undefined :: b))

alloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca = allocaBytes (sizeOf (undefined :: a))

allocaBytes :: forall a b . Int -> (Ptr a -> IO b) -> IO b
allocaBytes len io =
  mallocBytes len `primBind` (\ p ->
  io p `primBind` (\ b ->
  free p `primThen`
  primReturn b))
