module FArith(module FArith) where

import Prelude
import qualified Data.Double as D
import Text.String

list1 :: [D.Double]
list1 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

list2 :: [D.Double]
list2 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

divide :: D.Double -> D.Double -> D.Double
divide x y = if D.eqDouble y 0.0 then 0.0 else D.divDouble x y

main :: IO ()
main = do
  putStrLn $ show [ op x y | x <- list1, y <- list2, op <- [D.addDouble, D.subDouble, D.mulDouble, divide] ]
  putStrLn $ show [ op x y | x <- list1, y <- list2, op <- [D.eqDouble, D.neqDouble, D.ltDouble, D.leDouble, D.gtDouble, D.geDouble] ]
  putStrLn $ show [ D.divDouble x y | x <- [2.234983, 1.232, 23.0], y <- [1.0, 5.0, 10.0, 100.0]]
  putStrLn $ show [ D.divDouble x y | x <- [-2.234983, -1.232, -23.0], y <- [1.0, -5.0, 10.0, -100.0]]
  let str = readDouble "1.576"
  putStrLn $ show str
  putStrLn $ show $ D.addDouble 1.0 $ readDouble "2.5"
  putStrLn $ show $ map readDouble ["1.5e42", "1.2e-90"]
