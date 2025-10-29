module IOArray(main) where
import Mhs.MutArr
import Data.IORef
import System.IO
import System.IO.Serialize
default (String)

main :: IO ()
main = do
  a <- newMutIOArr 10 0
  s <- sizeMutIOArr a
  print s
  mapM_ (\ i -> unsafeWriteMutIOArr a i (i*i)) [0..9]
  xs <- mapM (unsafeReadMutIOArr a) [0..9]
  print xs
  o <- openFile "arr.tmp" WriteMode
  hSerialize o a
  hClose o
  i <- openFile "arr.tmp" ReadMode
  a' <- hDeserialize i
  xs' <- mapM (unsafeReadMutIOArr a') [0..9]
  print $ xs == xs'
  shrinkMutIOArr a 5
  s <- sizeMutIOArr a
  print s

  r <- newIORef "foo"
  s1 <- readIORef r
  print s1
  writeIORef r "bar"
  s2 <- readIORef r
  print s2
