module Fix where
import System.IO

main :: IO ()
main = do
  r <- fmap (take 10) . fixIO $
        \ fibs -> putStrLn "computing fibs" >> return (1 : 1 : zipWith (+) fibs (tail fibs))
  print r

