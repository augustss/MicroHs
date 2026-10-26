module Error56() where
f :: (forall a . a -> a) -> Int
f g = g 1
x :: (Int -> Int) -> Int
x = f
