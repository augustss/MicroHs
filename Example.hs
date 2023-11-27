module Example(fac, main) where
import Prelude

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n - 1)

main :: IO ()
main = do
  putStrLn $ "word size=" ++ show _wordSize ++ ", os=" ++ if _isWindows then "Windows" else "Unix-like"
  let
    rs = map fac [1,2,3,10]
  putStrLn "Some factorials"
  print rs
