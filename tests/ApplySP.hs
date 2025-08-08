module ApplySP where
import Foreign
import Foreign.C.String

foreign import ccall "hsasp.h hsasp" asp :: StablePtr (CString -> IO CString) -> IO ()

fcn :: CString -> IO CString
fcn cs = do
  s <- peekCString cs
  putStrLn $ "fcn: " ++ show s
  newCString "done"

main :: IO ()
main = do
  sp <- newStablePtr fcn
  asp sp
