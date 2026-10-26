module Error76() where
data T = C Int
f :: T -> Int
f (C x y) = x
