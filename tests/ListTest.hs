module ListTest(module ListTest) where

import Data.List

main :: IO ()
main = do
  print $ sum [1, 2, 3 :: Int]
  print $ product [1, 2, 3, 4 :: Int]
  print $ and [True]
  print $ and [True, False]
  print $ [2] `isInfixOf` [1, 2, 3 :: Int]
  -- elem and notElem are infix 4, so ++ binds tighter
  print $ 3 `elem` [1, 2] ++ [3 :: Int]
  print $ 3 `notElem` [1, 2] ++ [3 :: Int]
