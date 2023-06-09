module Example(module Example) where
import Prelude

fac n =
  if n <= 0 then
    1
  else
    n * fac(n-1)

main = do
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  putStrLn $ showList showInt rs
