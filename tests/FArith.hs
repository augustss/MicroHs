module FArith(module FArith) where

readFloat :: String -> Float
readFloat = read

-- Be careful about what constants we use here, so it works with 32 bit floats.
-- We only use values that can be represented exactly.

list1 :: [Float]
list1 = [-100.5::Float, -53.25::Float, 0.0::Float, 1.0::Float, 1.125::Float, 1.0e3::Float]

main :: IO ()
main = do
  print [ op x y | x <- list1, y <- list1, op <- [(+), (-), (*)] ]
  print [ op x y | x <- list1, y <- list1, op <- [(==), (/=), (<), (<=), (>), (>=)] ]
  print [ x / y  | x <- list1, y <- [1.0::Float, -16.0::Float, 0.0625::Float]]
  print $ readFloat "1.625"
  print $ 1.0 + readFloat "2.5"
  print $ map readFloat ["1.5e4", "12500.0e-4"]
  print (fromInteger 1000000000000000 :: Float)
