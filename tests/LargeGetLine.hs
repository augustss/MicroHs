module LargeGetLine where

import System.Mem (performGC, performGCWithReduction)

main :: IO ()
main = do
  getLine >> putStrLn "discarded"
  s <- getLine
  performGC
  performGCWithReduction
  print (s == take 1000000 (cycle "0123456789"))
  empty <- getLine
  print (null empty)
  end <- getLine
  print (end == "end")
