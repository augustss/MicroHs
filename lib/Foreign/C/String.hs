module Foreign.C.String(
  CString, CStringLen,
  newCAString, newCAStringLen,
  peekCAString, peekCAStringLen,
  withCAString,
  peekCString,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Char_Type
import Foreign.Marshal.Alloc

type CChar = Char
type CString = Ptr CChar
type CStringLen = (Ptr CChar, Int)

newCAString :: String -> IO CString
newCAString s = primNewCAStringLen s `primBind` \ (s, _) -> primReturn s

newCAStringLen :: String -> IO CStringLen
newCAStringLen = primNewCAStringLen

withCAString :: forall a . String -> (CString -> IO a) -> IO a
withCAString s io =
  newCAString s `primBind` \ cs ->
  io cs `primBind` \ a ->
  free cs `primThen`
  primReturn a

peekCAString :: CString -> IO String
peekCAString = primPeekCAString

peekCAStringLen :: CStringLen -> IO String
peekCAStringLen (p, i) = primPeekCAStringLen p i

-- Not quite right
peekCString :: CString -> IO String
peekCString = primPeekCAString

