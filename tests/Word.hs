module Word(main) where
import Prelude
import qualified Data.Word as W

main :: IO ()
main = do
  putStrLn $ show (4294967295::Int)
  putStrLn $ show (W.intToWord (1000::Int))
  putStrLn $ show twoTo32M1
  putStrLn $ show $ (*) twoTo32M1 twoTo32M1

twoTo32M1 :: W.Word
twoTo32M1 = W.intToWord (4294967295::Int)
