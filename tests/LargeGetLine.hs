module LargeGetLine where

import Control.Monad (replicateM_)
import System.IO
import System.Mem (performGC, performGCWithReduction)

main :: IO ()
main = do
  -- Use characters outside the small-integer table so that GC cannot merge
  -- their nodes.  This exposes the marking stack growth even with a small heap.
  withFile "LargeGetLine.tmp" WriteMode $ \h -> do
    replicateM_ 2 $ do
      replicateM_ 150000 (hPutStr h "\x100\x101")
      hPutChar h '\n'
    hPutStr h "\nend\n"
  withFile "LargeGetLine.tmp" ReadMode $ \h -> do
    hGetLine h >> putStrLn "discarded"
    s <- hGetLine h
    performGC
    performGCWithReduction
    print (s == take 300000 (cycle "\x100\x101"))
    empty <- hGetLine h
    print (null empty)
    end <- hGetLine h
    print (end == "end")
