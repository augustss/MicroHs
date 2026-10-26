module Error89() where
class C1 a where { m :: a }
class C2 a where { m :: a }
type C12 a = (C1 a, C2 a)
instance C12 Int where { m = 1 }
