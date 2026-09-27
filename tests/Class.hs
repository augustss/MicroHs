module Class(main) where
import Primitives

class Eqq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x /== y = not (x === y)

instance Eqq Int where
  (===) = primIntEQ

instance Eqq Char where
  (===) = primCharEQ

instance forall a . Eqq a => Eqq [a] where
  []     === []      =  True
  (x:xs) === (y:ys)  =  x === y && xs === ys
  _      === _       =  False

class (Eqq a) => Ordd a where
  (<==) :: a -> a -> Bool

instance Ordd Int where
  (<==) = (<=)

instance forall a b . (Eqq a, Eqq b) => Eqq (a, b) where
  (a, b) === (a', b')  =  a === a' && b === b'

f :: forall a . Eqq a => a -> Bool
f x = x === x

g :: forall a . Ordd a => a -> Bool
g x = x /== x

h :: forall a b . (Eqq a, Eqq b) => a -> b -> Bool
h a b = a === a && b === b

class A a b where
  a :: a -> b

class (A a a, A a a) => B1 a where
  b :: a -> a
  -- Multiple solutions
  b = a

class A a b => B a b
class A a b => C a b
class (B a b, C a b) => D a b where
  d :: a -> b
  -- Multiple solutions
  d = a

class E a where
  e :: a -> a

instance A Int Int where
  a x = x

-- Class synonym instances
type F a = E a
instance F Int where
  e x = x

-- Multiple instances at once
instance (B Int Int, C Int Int, D Int Int)

-- Multiple instances with methods
instance (A Char Char, E Char) where
  a x = x
  e x = x

-- tuple synonym
type ABCD a b = (A a b, B a b, C a b, D a b)
instance ABCD Bool Bool

-- constraints
type OrddT x y = (E x, Ordd (x,y))
instance forall a b . (Eqq a, Eqq b, Ord a, Ord b) => OrddT a b where
  (<==) = (<=)
  e x = x

-- cascading tuples
instance ((B Char Int, B Int Char), B Char Char, (A Int Char, A Char Int))

-- unrelated instances
class G a where
  g' :: a -> a
instance G Int where
  g' x = x
instance G Bool where
  g' = not
class G a => H a where
  h' :: a -> a
  h' = g'
instance (H Int, H Bool)

main :: IO ()
main = do
  print $ f (5::Int)
  print $ g (5::Int)
  print $ h (5::Int) 'a'
  print $ f [88::Int]
  print $ f (1::Int, 'a')
  print (a ('a'::Char)::Char)
  print (e ('b'::Char)::Char)
  print $ h' True
  print $ h' (1::Int)
