module StringTest(module StringTest) where
import Prelude

main :: IO ()
main = do
  putStrLn $ if (==) "abc" "abc" then "yes" else "no"
  putStrLn $ if (==) "abc" "adc" then "yes" else "no"
  putStrLn $ show (1234::Int)
  putStrLn $ show (0::Int)
  putStrLn $ show (- (567::Int))
  putStrLn $ show 'x'
  putStrLn $ show '\n'
  putStrLn $ show False
--  putStrLn $ showUnit ()
  putStrLn $ show [1,20,3::Int]
  putStrLn $ show [1::Int]
  putStrLn $ show ([] :: [Int])
  putStrLn $ show (123::Int, 'a')
  putStrLn $ show (Nothing :: Maybe Int)
  putStrLn $ show (Just 890 :: Maybe Int)
  putStrLn $ show (Left   678 :: Either Int Bool)
  putStrLn $ show (Right True :: Either Int Bool)
