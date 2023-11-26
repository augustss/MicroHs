module Dict(module Dict) where
import Prelude
import Data.Constraint

fac :: forall a . (Num a, Eq a) => a -> a
fac n = if n == 0 then 1 else n * fac(n - 1)

facD :: forall a . Dict (Num a, Eq a) -> a -> a
facD Dict = fac

dictInt :: Dict (Num Int, Eq Int)
dictInt = Dict

main :: IO ()
main = do
  print $ facD dictInt 10
  print $ withDict dictInt (fac (11::Int))
