module Main (main) where
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    _ <- forkIO (return ())
    print (BS.length (BS.pack (replicate 1000 'x')))   -- prints 1000  (correct)
    print (BS.length (BS.pack (replicate 5000 'y')))   -- prints 0     (expected 5000)
