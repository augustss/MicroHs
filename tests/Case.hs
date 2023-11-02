module Case(module Case) where
import Prelude

main :: IO ()
main = do
  putStrLn $ show $ f1 False
  putStrLn $ show  $ f2 False
  putStrLn $ show  $ f2 True
--  putStrLn $ showInt  $ f3 False
  putStrLn $ show $ map f4 [R,G,B]
  putStrLn $ show $ f5 [(3,4)]
  --putStrLn $ showInt $ f6 [(3,4)]
  putStrLn $ show $ [ i | Just i <- [Just 1, Nothing, Just 2] ]
  (x,y) <- return (2,3)
  putStrLn $ show $ x + y

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

instance Show RGB where
  show = showRGB

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

f5 :: [(Int, Int)] -> Int
f5 arg =
  case arg of
    [] -> 0
    (x,y) : _ -> x+y

{-
f6 :: [(Int, Int)] -> Int
f6 arg =
  case arg of
    [] -> 0
    (x,y) : ((u,v) : _) -> x+y
-}
