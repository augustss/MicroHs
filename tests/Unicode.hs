module Unicode(main) where
import Prelude

main :: IO ()
main = do
  putStrLn "abc"
  putStrLn "\xe5\&bc"
  putStrLn "\x402\xa88"
  writeFile "unicode.tst" "\xe5\&bc"
  ustr <- readFile "unicode.tst"
  print $ ustr == "\xe5\&bc"
