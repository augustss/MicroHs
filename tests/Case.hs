module Case(module Case) where
import Prelude

main :: IO ()
main = do
  putStrLn $ showBool $ f1 False
  putStrLn $ showInt  $ f2 False
  putStrLn $ showInt  $ f2 True
--  putStrLn $ showInt  $ f3 False
  putStrLn $ showList showRGB $ map f4 [R,G,B]

f1 :: Bool -> Bool
f1 b =
  case b of
    c -> c

f2 :: Bool -> Int
f2 b =
  case b of
    True -> 1
    _    -> 0

f3 :: Bool -> Int
f3 b =
  case b of
    True -> 1

data RGB = R | G | B

showRGB :: RGB -> String
showRGB c =
  case c of
    R -> "R"
    G -> "G"
    B -> "B"

nextRGB :: RGB -> RGB
nextRGB c =
  case c of
    R -> G
    G -> B
    B -> R

f4 :: RGB -> RGB
f4 c =
  case nextRGB c of
    R -> R
    k -> nextRGB k
