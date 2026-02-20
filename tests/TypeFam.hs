{-# LANGUAGE TypeFamilies #-}
module TypeFam where
import Data.Typeable

type family ItemS l

class IsListS l where
  fromListS :: [ItemS l] -> l
  toListS :: l -> [ItemS l]

type instance ItemS [a] = a

instance IsListS [a] where
  fromListS = id
  toListS = id

data ListInt = Nil | Cons Int ListInt
  deriving Show

type instance ItemS ListInt = Int

instance IsListS ListInt where
  fromListS [] = Nil
  fromListS (x:xs) = Cons x (fromListS xs)
  toListS Nil = []
  toListS (Cons x xs) = x : toListS xs

--------------


class IsList l where
  type Item l
  fromList :: [Item l] -> l
  toList :: l -> [Item l]


instance IsList [a] where
  type Item [a] = a
  fromList = id
  toList = id

instance IsList ListInt where
  type Item ListInt = Int
  fromList [] = Nil
  fromList (x:xs) = Cons x (fromListS xs)
  toList Nil = []
  toList (Cons x xs) = x : toListS xs

--------------

type family NonInj a b
type instance NonInj a b = b

nonInj1 :: NonInj Int Bool
nonInj1 = False
nonInj2 :: NonInj Bool Bool
nonInj2 = True
nonEq = [nonInj1, nonInj2]

nonInjInt :: NonInj Bool Int
nonInjInt = 1

{-
type family Inj a = r | r -> a
type instance Inj Int = Bool

ty :: forall a . Typeable a => Inj a -> TypeRep
ty _ = typeOf @a undefined
-}

main :: IO ()
main = do
  let l = [1,3,2] :: [Int]
  print (fromListS l :: [Int])
  print (fromListS l :: ListInt)
  print (fromList l :: [Int])
  print (fromList l :: ListInt)
--  print (ty True)
