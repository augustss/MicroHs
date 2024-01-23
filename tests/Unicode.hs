module Unicode(main) where
import Prelude

main :: IO ()
main = do
  putStrLn "abc"
  putStrLn "\xe5\&bc"
  putStrLn "\x402\xa88"
  writeFile "unicode.tmp" "\xe5\&bc"
  ustr <- readFile "unicode.tmp"
  print $ ustr == "\xe5\&bc"

foo ∷ ∀ a . Eq a ⇒ a → a
foo x = if x == x then x else undefined
