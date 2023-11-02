module Enum(main) where
import Prelude

main :: IO ()
main = do
  putStrLn $ show [1 .. 5]
  putStrLn $ show [1 .. 1]
  putStrLn $ show [1 .. 0]
  putStrLn $ show [1,3 .. 10]
  putStrLn $ show [1, -1 .. -5]
  putStrLn $ show $ take 5 [1 ..]
  putStrLn $ show $ take 5 [1,3 ..]
