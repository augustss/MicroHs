module FunDep(main) where
import Data.Char
import Data.Functor.Const

class C a b | a -> b where
  f :: a -> b

instance C Char Int where
  f = ord

class Plus a b c | a b -> c where
  plus :: a -> b -> c

instance Plus Int Int Int where
  plus = (+)

instance Plus Double Double Double where
  plus = (+)

instance Plus Int Double Double where
  plus x y = fromIntegral x + y

instance Plus Double Int Double where
  plus x y = x + fromIntegral y

class Iso a b | a -> b, b -> a where
  isoL :: a -> b
  isoR :: b -> a

instance Iso Char Int where
  isoL = ord
  isoR = chr

newtype T a = T a

instance C a b => C (T a) b where
  f (T a) = f a

------

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
type Getting r s a = (a -> Const r a) -> s -> Const r s

class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: Traversal s t (a,s) (b,t)

instance Cons [a] [b] a b where
  _Cons f (a:as) = uncurry (:) <$> f (a, as)
  _Cons _ []     = pure []

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

instance Field2 (a,b) (a,b') b b' where
  _2 k ~(a,b) = (\b' -> (a,b')) <$> k b

_tail :: Cons s s a a => Traversal' s s
_tail = _Cons._2

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)

{-
class D a where
  d :: a -> Int

instance D Char where
  d _ = 999

class E a

-- This currently not allowed (unbound b), but it could be.
class C a b => E a where
  e :: a -> a
  e = id

instance E Int
-}

class C3 aa bb cc | aa -> bb, bb -> cc where
  c3 :: aa -> bb -> cc

instance C3 Int Int Int where
  c3 = (+)

main :: IO ()
main = do
  print $ f 'a' + 1
  print $ plus (1::Int) (2::Int)
  print $ plus (1::Int) (2::Double)
  print $ plus (1::Double) (2::Int)
  print $ plus (1::Double) (2::Double)
  print $ isoL 'a'
  print $ isoR (100::Int)
  print $ f (T 'b')
  print $ ("abcd"::String)^._tail._tail
--  print $ d (f (e (0::Int)))
  print $ c3 (1::Int) 2
