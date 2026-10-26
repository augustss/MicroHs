module Error61() where
class C a where { m :: a -> Int }
instance C a => C (a, Int) where { m _ = 1 }
instance C (Int, b) where { m _ = 2 }
instance C Int where { m _ = 3 }
x = m (1::Int, 2::Int)
