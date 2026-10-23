module InterleaveIO where
import Data.IORef
import System.IO.Unsafe

main :: IO ()
main = do
  r <- newIORef (0 :: Int)
  let next = unsafeInterleaveIO (atomicModifyIORef r (\ n -> (n + 1, n)))
  a <- next
  b <- next
  c <- next
  print (a, b, c)
