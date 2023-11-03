module Rank2(main) where
import Prelude

f :: (forall a . a -> a) -> (Int, Bool)
f i = (i (1::Int), i True)

g :: (forall a . a -> Int -> a) -> (Int, Bool)
g c = (c (1::Int) (1::Int), c True (1::Int))

data Id = Id (forall a . a -> a)

iD :: Id
iD = Id (\ x -> x)

main :: IO ()
main = do
  putStrLn $ show $ f id
  putStrLn $ show $ g const
  case iD of
    Id i -> putStrLn $ show (i (1::Int), i True)
