module MutRec(main) where
import Prelude

main :: IO ()
main = do
  let even i = if i == 0 then True  else odd  (i - 1)
      odd  i = if i == 0 then False else even (i - 1)
  putStrLn $ show $ map even [1::Int .. 5] ++ map odd [1 .. 5]
