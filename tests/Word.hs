module Word(main) where
import Data.Word
import Numeric(showHex, showBin)

vals :: [Word16]
vals = [0xfff0, 0xfffe, 0xffff, 0, 1, 2, 5]

maxw :: Word
maxw = if _wordSize == 32 then 0x7fff::Word else 0x7fffffff::Word

hex :: Integral a => a -> String
hex x = showHex x ""

bin :: Integral a => a -> String
bin x = showBin x ""

main :: IO ()
main = do
  print (1000::Word)
  print $ maxw*maxw > 0
  print [ op x y | x <- vals, y <- vals, op <- [(+),( - ),(*)] ]
  print [ op x y | x <- vals, y <- vals, y /= 0, op <- [quot, rem] ]
  print [ op x y | x <- vals, y <- vals, op <- [(==),(/=),(<),(<=),(>),(>=)] ]
  print [ op x y | x <- vals, y <- vals, let op = compare ]
  putStrLn $ hex $ byteSwap16 0x1234
  putStrLn $ hex $ byteSwap32 0x12345678
  putStrLn $ hex $ byteSwap64 0x123456789abcdef0
  putStrLn $ bin $ bitReverse8 0b01001000
  putStrLn $ bin $ bitReverse16 0b0100_1000_0000_0001
  putStrLn $ bin $ bitReverse32 0b0100_1000_0000_0001_0000_1111_0000_0001
  putStrLn $ bin $ bitReverse64 0b0100_1000_0000_0001_0000_1111_0000_0001_0000000000000000000000000000001
