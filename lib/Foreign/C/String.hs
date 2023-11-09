module Foreign.C.String(
  CChar, CString,
  newCAString, withCAString,
  peekCAString,
  ) where
import Primitives
import Prelude

type CChar = Char
type CString = Ptr CChar

newCAString :: String -> IO CString
newCAString = primNewCAString

withCAString :: forall a . String -> (CString -> IO a) -> IO a
--withCAString s io =
--  newCAString s `primBind` \ cs -> io cs `primBind` \ a -> primFree cs `primThen` primReturn a
withCAString s io = do
  cs <- newCAString s
  a  <- io cs
  primFree cs
  return a

peekCAString :: CString -> IO String
peekCAString = primPeekCAString
