module TextLow where
import Data.Text
import Foreign
import Foreign.C

foreign import ccall "strlen" c_strlen :: Ptr CChar -> IO CSize
foreign import ccall "malloc" c_malloc :: CSize -> IO (Ptr CChar)
foreign import ccall "strcpy" c_strcpy :: Ptr CChar -> Ptr CChar -> IO ()

-- test some low level ugliness
main :: IO ()
main = do
  let s = "abc" :: Text
  print s
  xbs <- useAsCString s $ \ p -> do
    -- p is a pointer to the C string "abc"
    n <- c_strlen p
    q <- c_malloc (n+1)
    c_strcpy q p
    grabCString q
  print xbs

  -- Check that NUL survives going via a C string
  let s = "a\NULbc" :: Text
  print s
  xbs <- useAsCString s $ \ p -> do
    -- p is a pointer to the C string "a\xc0\x80bc"
    n <- c_strlen p
    q <- c_malloc (n+1)
    c_strcpy q p
    grabCString q
  print xbs
