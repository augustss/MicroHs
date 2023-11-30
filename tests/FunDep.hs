module FunDep(main) where
import Prelude

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

main :: IO ()
main = do
  print $ f 'a' + 1
  print $ plus (1::Int) (2::Int)
  print $ plus (1::Int) (2::Double)
  print $ plus (1::Double) (2::Int)
  print $ plus (1::Double) (2::Double)
  print $ isoL 'a'
  print $ isoR (100::Int)
