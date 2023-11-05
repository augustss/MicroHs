module Enum(main) where
import Prelude

main :: IO ()
main = do
  putStrLn $ show [1::Int .. 5]
  putStrLn $ show [1::Int .. 1]
  putStrLn $ show [1::Int .. 0]
  putStrLn $ show [1,3::Int .. 10]
  putStrLn $ show [1::Int, -1 .. -5]
  putStrLn $ show $ take 5 [1::Int ..]
  putStrLn $ show $ take 5 [1,3::Int ..]
