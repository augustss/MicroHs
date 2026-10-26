module Error59() where
f :: forall a . a -> a
f = (id :: Int -> Int)
