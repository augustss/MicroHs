module Foreign.C.String(
  CString, CStringLen,
  newCAString, newCAStringLen,
  peekCAString, peekCAStringLen,
  withCAString, withCAStringLen,
  newCString, newCStringLen,
  peekCString, peekCStringLen,
  withCString, withCStringLen,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Monad
import Data.ByteString.Internal as BS
import Data.Char_Type
import Data.Coerce(coerce)
import Data.Function
import Data.Num
import Data.Word.Word8
import Foreign.C.Types(CChar)
import Foreign.Ptr(Ptr, castPtr)
--import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Storable

type CString = Ptr CChar
type CStringLen = (Ptr CChar, Int)

-- The coercion is OK, because we will only use the lower 8 bits
-- of each Char.  That's what the A signifies.
-- String and [Word8] have the same representation.
stringToWord8s :: String -> [Word8]
stringToWord8s = primUnsafeCoerce
word8sToString :: [Word8] -> String
word8sToString = primUnsafeCoerce

newCAString :: String -> IO CString
newCAString s = packIO (stringToWord8s s) >>= getCString

newCAStringLen :: String -> IO CStringLen
newCAStringLen s = packIO (stringToWord8s s) >>= getCStringLen

getCStringLen :: ByteString -> IO (CString, Int)
getCStringLen bs = do
  p <- getCString bs
  let l = primBSlength bs
  l `seq` return (p, l)   -- evaluate l so we don't hold on to the bs

withCAString :: forall a . String -> (CString -> IO a) -> IO a
withCAString s io = do
  cs <- newCAString s
  a <- io cs
  free cs
  return a

withCAStringLen :: forall a . String -> (CStringLen -> IO a) -> IO a
withCAStringLen s io = do
  cs@(p, _) <- newCAStringLen s
  a <- io cs
  free p
  return a

peekCAString :: CString -> IO String
peekCAString cstr = do
  bs <- primPackCString cstr
  return (word8sToString (unpack bs))

peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (cstr, len) = do
  bs <- primPackCStringLen cstr len
  return (word8sToString (unpack bs))

------------------------------------------------------

newCString :: String -> IO CString
newCString s = packUTF8IO s >>= getCString

newCStringLen :: String -> IO CStringLen
newCStringLen s = packUTF8IO s >>= getCStringLen

withCString :: forall a . String -> (CString -> IO a) -> IO a
withCString s io = do
  cs <- newCString s
  a <- io cs
  free cs
  return a

withCStringLen :: forall a . String -> (CStringLen -> IO a) -> IO a
withCStringLen s io = do
  cs@(p, _) <- newCAStringLen s
  a <- io cs
  free p
  return a

peekCString :: CString -> IO String
peekCString cstr = do
  bs <- primPackCString cstr
  return (primBSfromUTF8 bs)

peekCStringLen :: CStringLen -> IO String
peekCStringLen (cstr, len) = do
  bs <- primPackCStringLen cstr len
  return (primBSfromUTF8 bs)


------------------------------------------------------

-- Copy the data from a bytestring and add a 0 at the end.
getCString :: ByteString -> IO (Ptr CChar)
--getCString = _primitive "bsgetptr"
getCString bs = 
  withForeignPtr (primBS2FPtr bs) $ \ p -> do   -- raw pointer to the bytestring
    let l = BS.length bs
    buf <- mallocBytes (l + 1)
    copyBytes buf p l                           -- copy the bytestring
    pokeByteOff buf l (0 :: Word8)              -- and add a trailing 0
    return (castPtr buf)

-- Avoid circular dependency
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fp io =
  io (primForeignPtrToPtr fp) `primBind` \ b ->
  primSeq fp (primReturn b)
