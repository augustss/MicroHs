module LocalPoly(main) where
import Prelude

main :: IO ()
main = do
  putStrLn $ showPair (showPair show show) (showPair show show) $ f 1 "a"

f :: forall b . Int -> b -> ((Int, b), (b, b))
f x b = (i x, i b)
  where
    i :: forall a . a -> (a, b)
    i a = (a, b)
