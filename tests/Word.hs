module Word(main) where
import Prelude
import Data.Word

main :: IO ()
main = do
  putStrLn $ show (4294967295::Int)
  putStrLn $ show (1000::Word)
  putStrLn $ show twoTo32M1
  putStrLn $ show $ (*) twoTo32M1 twoTo32M1

twoTo32M1 :: Word
twoTo32M1 = 4294967295::Word
