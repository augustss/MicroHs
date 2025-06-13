module Stable where
import Foreign.StablePtr

main :: IO ()
main = do
  sp1 <- newStablePtr (2 + 3)
  print sp1
  sp2 <- newStablePtr (4 + 3)
  print sp2
  deRefStablePtr sp2 >>= print
  deRefStablePtr sp1 >>= print
  freeStablePtr sp1
  newStablePtr 0 >>= print
