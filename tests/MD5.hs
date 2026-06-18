module MD5 where
import System.IO.MD5

main :: IO ()
main = do
  let md5 = md5String "The quick fox jumps over the lazy dog."
  print $ show md5 == "MD5 b05c16a5fb2573edab6cfcc41843b82e"
