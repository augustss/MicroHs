module FArith(module FArith) where

import Prelude
import Primitives
import qualified Data.Double as D
import Text.String

list1 :: [Double]
list1 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

list2 :: [Double]
list2 = [-100.343241, -53.3248973, -0.0, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

main :: IO ()
main = do
  putStrLn $ showList D.showDouble [ op x y | x <- list1, y <- list2, op <- [D.addDouble, D.subDouble, D.mulDouble] ]
  putStrLn $ showList showBool [ op x y | x <- list1, y <- list2, op <- [D.eqDouble, D.neqDouble, D.ltDouble, D.leDouble, D.gtDouble, D.geDouble] ]
  let str = readDouble "1.576"
  putStrLn $ D.showDouble str
  putStrLn $ D.showDouble $ D.addDouble 1.0 $ readDouble "2.5"