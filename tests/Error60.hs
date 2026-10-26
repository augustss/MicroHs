module Error60() where
g :: (forall a . a -> a) -> Int
g h = h 1
x = g (\y -> y + 1)
