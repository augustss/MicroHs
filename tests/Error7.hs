module Error7() where
type T a = (a, a)
data D (f :: Type -> Type) = D (f Int)
x :: D T
x = (0, 0)
