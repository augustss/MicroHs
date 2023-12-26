module Record(main) where
import Prelude

-- HasField instances are derived automatically

data R = CR { a :: Int, b :: Bool }

instance Show R where
  show (CR a b) = "R{a=" ++ show a ++ ",b=" ++ show b ++ "}"

data RR = CRR { r :: R, a :: Bool }

r1 :: R
r1 = CR { a=1, b=True }

r2 :: R
r2 = CR { b=True, a=2 }

r3 :: R
r3 = CR { b=True }

--r4 :: R
--r4 = R { c=True }

r5 :: R
r5 = r1 { a = (10::Int) }

r6 :: R
r6 = r1 { a = (10::Int), b=False }

rr1 :: RR
rr1 = CRR { r = r1, a = True }

sel_a :: forall r t . HasField "a" r t => r -> t
sel_a = (.a)

data S a = S1 { x :: Int } | S2 { x :: Int, y :: a }

instance forall a . Show a => Show (S a) where
  show (S1 x) = "S1 " ++ show x
  show (S2 x y) = "S2 " ++ show x ++ " " ++ show y

s1 :: S Bool
s1 = S1 10

s2 :: S String
s2 = S2 { x = 20, y = "foo" }

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
  print $ (.r.a) rr1
  print $ rr1.a
  print $ (.a) r1
  print $ sel_a r1
  print $ sel_a rr1
  print s1
  print s2
  print s1{x=99}
  print s2{x=88}
  print s2{y="bar"}
  