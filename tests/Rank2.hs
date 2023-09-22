module Rank2(main) where
import Prelude

f :: (forall a . a -> a) -> (Int, Bool)
f i = (i 1, i True)

g :: (forall a . a -> Int -> a) -> (Int, Bool)
g c = (c 1 1, c True 1)

main :: IO ()
main = do
  putStrLn $ showPair showInt showBool $ f id
  putStrLn $ showPair showInt showBool $ g const
