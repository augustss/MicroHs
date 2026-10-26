module Error92() where
f :: (forall a . a -> b -> a) -> b -> Int
f g x = g 1 x
y :: Int
y = f (\ a b -> b) 2
