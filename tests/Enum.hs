module Enum(main) where
import Prelude

main :: IO ()
main = do
  putStrLn $ showList showInt [1 .. 5]
  putStrLn $ showList showInt [1 .. 1]
  putStrLn $ showList showInt [1 .. 0]
  putStrLn $ showList showInt [1,3 .. 10]
  putStrLn $ showList showInt [1, -1 .. -5]
  putStrLn $ showList showInt $ take 5 [1 ..]
  putStrLn $ showList showInt $ take 5 [1,3 ..]
