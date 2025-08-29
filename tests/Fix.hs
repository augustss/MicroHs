module Fix where
import System.IO

main :: IO ()
main = do
  r <- fmap (take 10) . fixIO $
        \ fibs -> putStrLn "computing fibs" >> return (1 : 1 : zipWith (+) fibs (tail fibs))
  print r

  rec { a <- return (b+1)
      ; b <- return 2 }
  print (a, b)

