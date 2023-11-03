module ListTest(module ListTest) where
import Prelude

main :: IO ()
main = do
  putStrLn $ show $ sum [1,2,3::Int]
  putStrLn $ show $ product [1,2,3,4::Int]
  putStrLn $ show $ and [True]
  putStrLn $ show $ and [True, False]
