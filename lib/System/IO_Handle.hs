module System.IO_Handle(BFILE, Handle, mkHandle, withHandle, killHandle, unsafeHandle) where
import Prelude()
import Primitives

-- A handle is a ForeignPtr to a C BFILE transducer.
-- It needs to be a ForeignPtr so it can have a finalizer
-- that closes the underlying BFILE when the Handle is gc():ed.

data BFILE
newtype Handle = Handle (ForeignPtr BFILE)

unsafeHandle :: ForeignPtr BFILE -> Handle
unsafeHandle = Handle

withHandle :: Handle -> (Ptr BFILE -> IO a) -> IO a
withHandle (Handle fp) io =
  io (primForeignPtrToPtr fp) `primBind` \ a ->
  primSeq fp (primReturn a)  -- hold on to fp so it's not gc():ed

foreign import ccall "&closeb" c_close :: FunPtr (Ptr BFILE -> IO ())

-- Create a Handle with the appropriate finalizer.
mkHandle :: Ptr BFILE -> IO Handle
mkHandle p =
  primNewForeignPtr p `primBind` \ fp ->
  primAddFinalizer c_close fp `primThen`
  primReturn (Handle fp)

-- When a handle is closed, we must remove the c_close finalizer.
killHandle :: Handle -> IO ()
killHandle (Handle fp) =
  primAddFinalizer (primIntToFunPtr (0::Int)) fp