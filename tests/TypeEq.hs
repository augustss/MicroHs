module TypeEq(main) where
import Prelude

foo :: forall a . (a ~ Bool) => a -> a
foo = not

data Exp a
  = (a ~ Int)  => Int Int
  | (a ~ Int)  => Add (Exp Int) (Exp Int)
  | (a ~ Bool) => Equ (Exp Int) (Exp Int)
  |               Iff (Exp Bool) (Exp a) (Exp a)

eval :: forall a . Exp a -> a
eval (Int i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Equ e1 e2) = eval e1 == eval e2
eval (Iff c e1 e2) = if eval c then eval e1 else eval e2

e1 :: Exp Int
e1 = Iff (Add (Int 1) (Int 2) `Equ` Int 3) (Int 1) (Int 999)

main :: IO ()
main = do
  print (foo True)
  print (eval e1)
