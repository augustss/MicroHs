module LocalPoly(main) where

main :: IO ()
main = do
  print $ f 1 ("a"::String)
  print $ g 1 ("a"::String)

f :: forall b . Int -> b -> ((Int, b), (b, b))
f x b = (i x, i b)
  where
    i :: forall a . a -> (a, b)
    i a = (a, b)

g :: forall b . Int -> b -> ((Int, b), (b, b))
g x b = (i x, i b)
  where
    i a = (a, b)
