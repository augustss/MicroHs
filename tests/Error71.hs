module Error71() where
f :: Int -> Int
f x = do { y <- Just x; return y }
