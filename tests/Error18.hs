module Error18() where
data T = forall a . C a
x :: Int
x = let { C a = C (1::Int) } in a
