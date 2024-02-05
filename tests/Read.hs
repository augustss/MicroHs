module Read(main) where
import Prelude
import Text.Read

main :: IO ()
main = do
  print (read "123"   :: Int)
  print (read " 123"  :: Int)
  print (read "123 "  :: Int)
  print (read "-123"  :: Int)
  print (read "(123)" :: Int)
  print (read "2147483647" :: Int)
  print (read "-2147483648" :: Int)
  if _wordSize == 64 then do
    print (read "9223372036854775807" :: Int)
    print (read "-9223372036854775808" :: Int)
   else do
    putStrLn "9223372036854775807"
    putStrLn "-9223372036854775808"
  print (read "123"   :: Integer)
  print (read " 123"  :: Integer)
  print (read "123 "  :: Integer)
  print (read "-123"  :: Integer)
  print (read "(123)" :: Integer)
  print (read "1234567890123456789012345678901234567890" :: Integer)
  print (read "[1,2,3]" :: [Int])
  print (read "False" :: Bool)
  print (read "True" :: Bool)
  print (reads "123 4" :: [(Int, String)])
  print (readMaybe "123" :: Maybe Int)
  print (readMaybe "apa" :: Maybe Int)
