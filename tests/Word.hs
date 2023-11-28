module Word(main) where
import Prelude
import Data.Word

main :: IO ()
main = do
  putStrLn $ show (1000::Word)
  putStrLn $ show $ maxw*maxw > 0

maxw :: Word
maxw = if _wordSize == 32 then 0x7fff::Word else 0x7fffffff::Word
