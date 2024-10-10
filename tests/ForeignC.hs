module ForeignC(main) where
import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "llabs" iabs :: Int -> IO Int
foreign import ccall "sys/errno.h &errno" cerrno :: IO (Ptr CInt)
foreign import ccall "&llabs" pabs :: IO (FunPtr (Int -> IO Int))

main :: IO ()
main = do
  x1 <- iabs (3 - 8)
  putStrLn $ show x1
  x2 <- iabs (10 - 8)
  putStrLn $ show x2
  p <- cerrno
  CInt e <- peek p
  print e
