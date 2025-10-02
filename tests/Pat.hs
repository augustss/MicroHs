module Pat where

import qualified Data.List

myHead :: [a] -> a
myHead (x Data.List.: _) = x

main :: IO ()
main = print $ myHead [1, 2, 3]
