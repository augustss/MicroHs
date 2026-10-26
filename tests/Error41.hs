module Error41() where
s :: Int -> Int
s x = foldr (\a b -> a + b) 0 x
