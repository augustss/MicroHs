module Example(module Example) where
import Prelude
import Data.List
import System.IO
import String

fac n =
  case n <= 0 of
    False -> n * fac(n-1)
    True  -> 1

main = do
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  putStrLn $ showList showInt rs
