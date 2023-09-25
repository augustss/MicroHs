module MutRec(main) where
import Prelude

main :: IO ()
main = do
  let even i = if i == 0 then True  else odd  (i - 1)
      odd  i = if i == 0 then False else even (i - 1)
  putStrLn $ showList showBool $ map even [1 .. 5] ++ map odd [1 .. 5]
