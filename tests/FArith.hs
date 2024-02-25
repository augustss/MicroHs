module FArith(module FArith) where
import Prelude

readDouble :: String -> Double
readDouble = read

-- Be careful about what constants we use here, so it works with both 32 and 64 bit floats.
-- We only use values that can be represented exactly.

list1 :: [Double]
list1 = [-100.25::Double, -53.25::Double, 0.0::Double, 1.0::Double, 1.125::Double, 1.0e4::Double]

main :: IO ()
main = do
  putStrLn $ show [ op x y | x <- list1, y <- list1, op <- [(+), (-), (*)] ]
  putStrLn $ show [ op x y | x <- list1, y <- list1, op <- [(==), (/=), (<), (<=), (>), (>=)] ]
  putStrLn $ show [ x / y  | x <- list1, y <- [1.0::Double, -16.0::Double, 0.0625::Double]]
  putStrLn $ show $ readDouble "1.565"
  putStrLn $ show $ 1.0 + readDouble "2.5"
  putStrLn $ show $ map readDouble ["1.5e32", "1.25e-33"]
