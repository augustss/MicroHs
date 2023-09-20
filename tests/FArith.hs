module FArith(module FArith) where

import Data.Double
import Prelude

list1 :: [Double]
list1 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

list2 :: [Double]
list2 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

main :: IO ()
main = do
  putStrLn $ showList showDouble [ op x y | x <- list1, y <- list2, op <- [addDouble, subDouble, mulDouble] ]
  putStrLn $ showList showBool [ op x y | x <- list1, y <- list2, op <- [eqDouble, neqDouble, ltDouble, leDouble, gtDouble, geDouble] ]