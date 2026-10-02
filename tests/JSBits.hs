module JSBits where
import JSBitsLib

main :: IO ()
main = do
  n <- jsBitsAdd 40 2
  print n
