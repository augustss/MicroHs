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
import Data.ByteString.Internal
import Data.Char_Type
import Data.Coerce(coerce)
import Foreign.C.Types(CChar)
import Foreign.Marshal.Alloc

type CString = Ptr CChar
type CStringLen = (Ptr CChar, Int)

-- The coercion is OK, because we will only use the lower 8 bits
-- of each Char.  That's what the A signifies.
newCAString :: String -> IO CString
newCAString s = packIO (primUnsafeCoerce s) `primBind` getBSPtr

newCAStringLen :: String -> IO CStringLen
newCAStringLen s = packIO (primUnsafeCoerce s) `primBind` getBSPtrLen

getBSPtrLen :: ByteString -> IO (CString, Int)
getBSPtrLen bs =
  let l = primBSlength bs  -- carefully get the length before getPtr zaps it
  in  l `primSeq` (primBSgetPtr bs `primBind` \ p -> primReturn (p, l))

getBSPtr :: ByteString -> IO CString
getBSPtr = primBSgetPtr

withCAString :: forall a . String -> (CString -> IO a) -> IO a
withCAString s io =
  newCAString s `primBind` \ cs ->
  io cs `primBind` \ a ->
  free cs `primThen`
  primReturn a

withCAStringLen :: forall a . String -> (CStringLen -> IO a) -> IO a
withCAStringLen s io =
  newCAStringLen s `primBind` \ cs@(p, _) ->
  io cs `primBind` \ a ->
  free p `primThen`
  primReturn a

peekCAString :: CString -> IO String
peekCAString cstr = primPackCString cstr `primBind` \bs -> primReturn (primUnsafeCoerce unpack bs)

peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (cstr, len) = primPackCStringLen cstr len `primBind` \bs -> primReturn (primUnsafeCoerce unpack bs)

------------------------------------------------------
-- XXX:  No encoding!

newCString :: String -> IO CString
newCString = newCAString

newCStringLen :: String -> IO CStringLen
newCStringLen = newCAStringLen

withCString :: forall a . String -> (CString -> IO a) -> IO a
withCString = withCAString

withCStringLen :: forall a . String -> (CStringLen -> IO a) -> IO a
withCStringLen = withCAStringLen

peekCString :: CString -> IO String
peekCString = peekCAString

peekCStringLen :: CStringLen -> IO String
peekCStringLen = peekCAStringLen
