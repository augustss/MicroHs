module Foreign(main) where
import Prelude

foreign import ccall "llabs" iabs :: Int -> IO Int

main :: IO ()
main = do
  x1 <- iabs (3 - 8)
  putStrLn $ show x1
  x2 <- iabs (10 - 8)
  putStrLn $ show x2
