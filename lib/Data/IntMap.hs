-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.IntMap(
  IntMap,
  empty, lookup, insert, fromList, toList, insertWith, (!), keys
  ) where
import Prelude hiding(lookup)

data IntMap a
  = Empty
  | Leaf Int a
  | Node (IntMap a) (IntMap a) (IntMap a) (IntMap a)
  --Xderiving (Show)

-- This works for y>0
divModX :: Int -> Int -> (Int, Int)
divModX x y =
  let
    q = quot x y
    r = rem x y
  in
    if x >= 0 || r == 0 then
      (q, r)
    else
      (q - 1, r + y)

empty :: forall a . IntMap a
empty = Empty

lookup :: forall a . Int -> IntMap a -> Maybe a
lookup k am =
  case am of
    Empty -> Nothing
    Leaf i a -> if k == i then Just a else Nothing
    Node m0 m1 m2 m3 ->
      let
        (d, m) = divModX k 4
      in      if m == 0 then lookup d m0
         else if m == 1 then lookup d m1
         else if m == 2 then lookup d m2
         else                lookup d m3

insert :: forall a . Int -> a -> IntMap a -> IntMap a
insert = insertWith const

fromList :: forall a . [(Int, a)] -> IntMap a
fromList = foldr (uncurry insert) empty

-- XXX There must be a better way
toList :: forall a . IntMap a -> [(Int, a)]
toList am =
  let
    f o (k, a) = (k*4 + o, a)
  in
    case am of
      Empty -> []
      Leaf l a -> [(l, a)]
      Node m0 m1 m2 m3 ->
        map (f 0) (toList m0) `merge`
        map (f 1) (toList m1) `merge`
        map (f 2) (toList m2) `merge`
        map (f 3) (toList m3)

merge :: forall a . [(Int, a)] -> [(Int, a)] -> [(Int, a)]
merge [] ys = ys
merge xs [] = xs
merge xxs@(xa@(x,_):xs) yys@(yb@(y,b):ys) = if x < y then xa : merge xs yys else yb : merge xxs ys

keys :: forall a . IntMap a -> [Int]
keys = map fst . toList

(!) :: forall a . IntMap a -> Int -> a
(!) m k =
  case lookup k m of
    Just i -> i
    Nothing -> error "Data.IntMap.!"

insertWith :: forall a . (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith comb ak a =
  let
    ins k am =
      case am of
        Empty -> Leaf k a
        Leaf i b ->
          if k == i then
            Leaf k (comb a b)
          else
            ins k $ insert i b $ Node Empty Empty Empty Empty
        Node m0 m1 m2 m3 ->
          let
            (d, m) = divModX k 4
          in      if m == 0 then Node (ins d m0) m1 m2 m3
             else if m == 1 then Node m0 (ins d m1) m2 m3
             else if m == 2 then Node m0 m1 (ins d m2) m3
             else                Node m0 m1 m2 (ins d m3)
  in ins ak
