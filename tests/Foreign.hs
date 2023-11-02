module Foreign(main) where
import Prelude

foreign import ccall "llabs" abs :: Int -> IO Int

main :: IO ()
main = do
  x1 <- abs (3 - 8)
  putStrLn $ show x1
  x2 <- abs (10 - 8)
  putStrLn $ show x2
