module StringTest(module StringTest) where
import Prelude

main :: IO ()
main = do
  putStrLn $ showInt 1234
  putStrLn $ showInt 0
  putStrLn $ showInt (negate 567)
  putStrLn $ showChar 'x'
  putStrLn $ showChar '\n'
  putStrLn $ showBool False
--  putStrLn $ showUnit ()
  putStrLn $ showList showInt [1,20,3]
  putStrLn $ showList showInt [1]
  putStrLn $ showList showInt []
  putStrLn $ showPair showInt showChar (123, 'a')
  putStrLn $ showMaybe showInt Nothing
  putStrLn $ showMaybe showInt (Just 890)
  putStrLn $ showEither showInt showBool (Left 678)
  putStrLn $ showEither showInt showBool (Right True)
