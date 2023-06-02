module Example where
import Prelude
import List
import IO
import String

fac n =
  case n <= 0 of
    False -> n * fac(n-1)
    True  -> 1

main = IO.do
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  putStrLn $ showList showInt rs
