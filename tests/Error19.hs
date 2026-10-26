module Error19() where
class C a where { m :: a }
instance C Int where { x = 1 }
