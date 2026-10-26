module Error90(f) where
f :: Int -> Int
f x | rec { y <- Just x } = x
