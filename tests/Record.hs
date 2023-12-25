module Record(main) where
import Prelude

data R = R { a :: Int, b :: Bool }

instance HasField "a" R Int where
  hasField _ = has_a
instance HasField "b" R Bool where
  hasField _ = has_b

has_a :: R -> (Int, Int -> R)
has_a (R a b) = (a, \ a' -> R a' b)

has_b :: R -> (Bool, Bool -> R)
has_b (R a b) = (b, \ b' -> R a b')

instance Show R where
  show (R a b) = "R{a=" ++ show a ++ ",b=" ++ show b ++ "}"

data RR = RR { r :: R }
instance HasField "r" RR R where
  hasField _ (RR r) = (r, RR)


r1 :: R
r1 = R { a=1, b=True }

r2 :: R
r2 = R { b=True, a=2 }

r3 :: R
r3 = R { b=True }

--r4 :: R
--r4 = R { c=True }

r5 :: R
r5 = r1 { a = (10::Int) }

r6 :: R
r6 = r1 { a = (10::Int), b=False }

rr1 :: RR
rr1 = RR { r = r1 }

main :: IO ()
main = do
  print r1
  print r2
--  print r3
  print r5
  print r6
  print $ r2.a
  print $ r2.b
  print $ rr1.r.a
  print $ (.a) r1

{-
data Person = Person { name :: String }

instance HasField "name" Person String where
  getField _ (Person n) = n

pers :: Person
pers = Person "foo"

main :: IO ()
main = do
  print $ getField (Proxy :: Proxy "name") pers
-}
