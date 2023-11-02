module StringTest(module StringTest) where
import Prelude

main :: IO ()
main = do
  putStrLn $ if (==) "abc" "abc" then "yes" else "no"
  putStrLn $ if (==) "abc" "adc" then "yes" else "no"
  putStrLn $ show 1234
  putStrLn $ show 0
  putStrLn $ show (negate 567)
  putStrLn $ show 'x'
  putStrLn $ show '\n'
  putStrLn $ show False
--  putStrLn $ showUnit ()
  putStrLn $ show [1,20,3]
  putStrLn $ show [1]
  putStrLn $ show ([] :: [Int])
  putStrLn $ showPair show show (123, 'a')
  putStrLn $ showMaybe show (Nothing :: Maybe Int)
  putStrLn $ showMaybe show (Just 890)
  putStrLn $ showEither show show (Left   678 :: Either Int Bool)
  putStrLn $ showEither show show (Right True :: Either Int Bool)
