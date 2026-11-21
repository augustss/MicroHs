-- Run machine code generated at run time: map a page, write the code to
-- it, make it executable, and call it through a FunPtr.
-- The code is for arm64, and the page is mapped for macOS: MAP_JIT is
-- required under the hardened runtime (together with the entitlement
-- com.apple.security.cs.allow-jit).  On Linux, drop mapJit.
module JitCall(main) where

import Control.Exception (bracket)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, nullPtr)
import System.Posix.Types (COff(..))

foreign import capi "sys/mman.h value PROT_READ"   protRead   :: CInt
foreign import capi "sys/mman.h value PROT_WRITE"  protWrite  :: CInt
foreign import capi "sys/mman.h value PROT_EXEC"   protExec   :: CInt
foreign import capi "sys/mman.h value MAP_PRIVATE" mapPrivate :: CInt
foreign import capi "sys/mman.h value MAP_ANON"    mapAnon    :: CInt
foreign import capi "sys/mman.h value MAP_JIT"     mapJit     :: CInt
foreign import capi "sys/mman.h value MAP_FAILED"  mapFailed  :: Ptr a
foreign import capi "unistd.h value _SC_PAGESIZE"  scPageSize :: CInt

foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)
foreign import ccall "sys/mman.h mprotect"
  c_mprotect :: Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr a -> CSize -> IO CInt
foreign import ccall "unistd.h sysconf"
  c_sysconf :: CInt -> IO CLong

-- The instruction cache must forget what was at the addresses before.
foreign import ccall "libkern/OSCacheControl.h sys_icache_invalidate"
  icacheInvalidate :: Ptr a -> CSize -> IO ()

foreign import ccall "dynamic"
  callIntFun :: FunPtr (IO CInt) -> IO CInt

-- arm64 instructions
movW0 :: Word32 -> Word32      -- mov w0, #imm, for a 16 bit imm
movW0 imm = 0x52800000 .|. (imm `shiftL` 5)

ret :: Word32
ret = 0xd65f03c0

-- Run code that returns an int.
runCode :: [Word32] -> IO CInt
runCode code = do
  size <- fromIntegral <$> throwErrnoIfMinus1 "sysconf" (c_sysconf scPageSize)
  bracket (mapPage size) (unmapPage size) $ \ page -> do
    pokeArray page code
    icacheInvalidate page size
    throwErrnoIfMinus1_ "mprotect" $ c_mprotect page size (protRead .|. protExec)
    callIntFun (castPtrToFunPtr page)

mapPage :: CSize -> IO (Ptr Word32)
mapPage size =
  throwErrnoIf (== mapFailed) "mmap" $
    c_mmap nullPtr size (protRead .|. protWrite) (mapPrivate .|. mapAnon .|. mapJit) (-1) 0

unmapPage :: CSize -> Ptr Word32 -> IO ()
unmapPage size page = throwErrnoIfMinus1_ "munmap" $ c_munmap page size

main :: IO ()
main = do
  putStrLn "About to jump into generated code"
  print =<< runCode [movW0 42, ret]
  putStrLn "Done"
