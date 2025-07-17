module DArith(module DArith) where

readDouble :: String -> Double
readDouble = read

list1 :: [Double]
list1 = [-100.5::Double, -53.25::Double, 0.0::Double, 1.0::Double, 1.125::Double, 1.0e3::Double]

main :: IO ()
main = do
  print [ op x y | x <- list1, y <- list1, op <- [(+), (-), (*)] ]
  print [ op x y | x <- list1, y <- list1, op <- [(==), (/=), (<), (<=), (>), (>=)] ]
  print [ x / y  | x <- list1, y <- [1.0::Double, -16.0::Double, 0.0625::Double]]
  print $ readDouble "1.625"
  print $ 1.0 + readDouble "2.5"
  print $ map readDouble ["1.5e4", "12500.0e-4"]
  print (fromInteger 1000000000000000 :: Double)
