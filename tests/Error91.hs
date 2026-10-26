module Error91(g) where
data R a = R { f1 :: a }
g :: R Int -> R Int
g r = r { f1 = 1, f2 = 2 }
