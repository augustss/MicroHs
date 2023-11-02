module Word(main) where
import Prelude
import qualified Data.Word as W

main :: IO ()
main = do
  putStrLn $ show 4294967295
  putStrLn $ show (W.intToWord 1000)
  putStrLn $ show twoTo32M1
  putStrLn $ show $ (W.*) twoTo32M1 twoTo32M1

twoTo32M1 :: W.Word
twoTo32M1 = W.intToWord 4294967295
