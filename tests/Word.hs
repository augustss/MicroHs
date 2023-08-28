module Word(main) where
import Prelude
import qualified Data.Word as W

main :: IO ()
main = do
  putStrLn $ showInt 4294967295
  putStrLn $ W.showWord (W.intToWord 1000)
  putStrLn $ W.showWord twoTo32M1
  putStrLn $ W.showWord $ (W.*) twoTo32M1 twoTo32M1

twoTo32M1 :: Word
twoTo32M1 = W.intToWord 4294967295

