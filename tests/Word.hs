module Word(main) where
import Prelude
import Data.Word

vals :: [Word16]
vals = [0xfff0, 0xfffe, 0xffff, 0, 1, 2, 5]

maxw :: Word
maxw = if _wordSize == 32 then 0x7fff::Word else 0x7fffffff::Word

main :: IO ()
main = do
  putStrLn $ show (1000::Word)
  putStrLn $ show $ maxw*maxw > 0
  putStrLn $ show [ op x y | x <- vals, y <- vals, op <- [(+),( - ),(*)] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, y /= 0, op <- [quot, rem] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, op <- [(==),(/=),(<),(<=),(>),(>=)] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, let op = compare ]
