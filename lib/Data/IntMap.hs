-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.IntMap(module Data.IntMap) where
import Prelude --Xhiding(lookup)

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
      (q-1, r+y)

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
insert ak a =
  let
    ins k am =
      case am of
        Empty -> Leaf k a
        Leaf i b ->
          if k == i then
            Leaf k a
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

fromList :: forall a . [(Int, a)] -> IntMap a
fromList = foldr (uncurry insert) empty

toList :: forall a . IntMap a -> [(Int, a)]
toList am =
  let
    f o (k, a) = (k*4 + o, a)
  in
    case am of
      Empty -> []
      Leaf l a -> [(l, a)]
      Node m0 m1 m2 m3 ->
        map (f 0) (toList m0) ++
        map (f 1) (toList m1) ++
        map (f 2) (toList m2) ++
        map (f 3) (toList m3)
