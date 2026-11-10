module IOEFileName(main) where
import Control.Exception
import System.IO
import System.IO.Error

main :: IO ()
main = do
  r <- try (openFile "nope.tmp/x" ReadMode)
  case r of
    Left e -> do
      print (ioeGetFileName e)
      putStrLn (ioeGetLocation e)
      print (isDoesNotExistError e)
      print e
    Right _ -> putStrLn "no error"
