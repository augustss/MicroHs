{-# LANGUAGE TypeFamilies #-}
module TypeFam where

type family Item l

class IsList l where
  fromList :: [Item l] -> l
  toList :: l -> [Item l]

type instance Item [a] = a

instance IsList [a] where
  fromList = id
  toList = id

data ListInt = Nil | Cons Int ListInt
  deriving Show

type instance Item ListInt = Int

instance IsList ListInt where
  fromList [] = Nil
  fromList (x:xs) = Cons x (fromList xs)
  toList Nil = []
  toList (Cons x xs) = x : toList xs

{-
instance IsList (ZipList a) where
  fromList = ZipList
  toList = getZipList

instance IsList (NonEmpty a) a where
  fromList (a:as) = a :| as
  fromList [] = error "NonEmpty.fromList: empty list"

  toList ~(a :| as) = a : as
-}

main :: IO ()
main = do
  let l = [1,3,2] :: [Int]
  print (fromList l :: [Int])
  print (fromList l :: ListInt)
