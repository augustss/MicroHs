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

  x <- mdo
         a <- return (b+1)
         b <- return (c+1)
         let c = d+1
         d <- return 42
         return (a+b)
  print x
