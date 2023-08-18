-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Inspired by https://sortingsearching.com/2020/05/23/2-3-trees.html
--
module MicroHs.StringMapFast(module MicroHs.StringMapFast) where
import Prelude --Xhiding(lookup)
--Ximport Compat

data Map v
  = Empty
  | Leaf String v
  | Node2 Int String (Map v) (Map v)
  | Node3 Int String (Map v) (Map v) (Map v)
  --Xderiving (Show)

data OneOrTwo a
  = OOT1 a
  | OOT2 a a
  --Xderiving (Show)

height :: forall v . Map v -> Int
height m =
  case m of
    Empty -> undefined
    Leaf _ _ -> 0
    Node2 h _ _ _ -> h
    Node3 h _ _ _ _ -> h

smallest :: forall v . Map v -> String
smallest m =
  case m of
    Empty -> undefined
    Leaf k _ -> k
    Node2 _ k _ _ -> k
    Node3 _ k _ _ _ -> k

replSmallest :: forall v . (v -> v) -> Map v -> Map v
replSmallest f m =
  case m of
    Empty -> undefined
    Leaf k v -> Leaf k (f v)
    Node2 h s a b -> Node2 h s (replSmallest f a) b
    Node3 h s a b c -> Node3 h s (replSmallest f a) b c

node2 :: forall v . Map v -> Map v -> Map v
node2 a b = Node2 (height a + 1) (smallest a) a b

node3 :: forall v . Map v -> Map v -> Map v -> Map v
node3 a b c = Node3 (height a + 1) (smallest a) a b c

meld :: forall v . OneOrTwo (Map v) -> OneOrTwo (Map v) -> OneOrTwo (Map v)
meld m1 m2 =
  case m1 of
    OOT1 a ->
      case m2 of
        OOT1 b -> OOT1 $ node2 a b
        OOT2 b c -> OOT1 $ node3 a b c
    OOT2 a b ->
      case m2 of
        OOT1 c -> OOT1 $ node3 a b c
        OOT2 c d -> OOT2 (node2 a b) (node2 c d)

mergeToSameHeight :: forall v . Map v -> Map v -> OneOrTwo (Map v)
mergeToSameHeight a b =
  if height a < height b then
    case b of
      Node2 _ _ b1 b2 -> meld (mergeToSameHeight a b1) (OOT1 b2)
      Node3 _ _ b1 b2 b3 -> meld (mergeToSameHeight a b1) (OOT2 b2 b3)
      _ -> undefined
  else if height a > height b then
    case a of
      Node2 _ _ a1 a2 -> meld (OOT1 a1) (mergeToSameHeight a2 b)
      Node3 _ _ a1 a2 a3 -> meld (OOT2 a1 a2) (mergeToSameHeight a3 b)
      _ -> undefined
  else
    OOT2 a b

-- All elements in aa smaller than elements in ab
merge :: forall v . Map v -> Map v -> Map v
merge aa ab =
  case aa of
    Empty -> ab
    _ ->
      case ab of
        Empty -> aa
        _ ->
          case mergeToSameHeight aa ab of
            OOT1 t -> t
            OOT2 t u -> node2 t u

split :: forall v . (String -> Bool) -> Map v -> (Map v, Map v)
split f am =
  case am of
    Empty -> (Empty, Empty)
    Leaf k _ ->
      if f k then
        (Empty, am)
      else
        (am, Empty)
    Node2 _ _ a b ->
      if f (smallest b) then
        case split f a of
          (a1,a2) -> (a1, merge a2 b)
      else
        case split f b of
          (b1,b2) -> (merge a b1, b2)
    Node3 _ _ a b c ->
      if f (smallest b) then
        case split f a of
          (a1,a2) -> (a1, merge a2 (node2 b c))
      else if f (smallest c) then
        case split f b of
          (b1,b2) -> (merge a b1, merge b2 c)
      else
        case split f c of
          (c1,c2) -> (merge (node2 a b) c1, c2)

-----------------------------------------

insertWith :: forall v . (v -> v -> v) -> String -> v -> Map v -> Map v
insertWith f k v a =
  case split (leString k) a of
    (a1, a2) ->
      case a2 of
        Empty -> merge a1 (Leaf k v)
        _ ->
          if leString (smallest a2) k then
            merge a1 (replSmallest (f v) a2)
          else
            merge (merge a1 (Leaf k v)) a2

insert :: forall v . String -> v -> Map v -> Map v
insert = insertWith const

lookup :: forall v . String -> Map v -> Maybe v
lookup x am =
  case am of
    Empty -> Nothing
    Leaf k v -> if leString k x && leString x k then Just v else Nothing
    Node2 _ _ a b ->
      if leString (smallest b) x then
        lookup x b
      else
        lookup x a
    Node3 _ _ a b c ->
      if leString (smallest c) x then
        lookup x c
      else if leString (smallest b) x then
        lookup x b
      else
        lookup x a

union :: forall v . Map v -> Map v -> Map v
union m1 m2 = foldr (uncurry insert) m2 (toList m1)

fromListWith :: forall v . (v -> v -> v) -> [(String, v)] -> Map v
fromListWith f = foldr (uncurry (insertWith f)) Empty

toList :: forall v . Map v -> [(String, v)]
toList m =
  let
    pre aa xs =
      case aa of
        Empty -> xs
        Leaf k v -> (k, v) : xs
        Node2 _ _ a b -> pre a (pre b xs)
        Node3 _ _ a b c -> pre a (pre b (pre c xs))
  in pre m []

fromList :: forall v . [(String, v)] -> Map v
fromList = fromListWith const

empty :: forall v . Map v
empty = Empty

elems :: forall v . Map v -> [v]
elems = map snd . toList

size :: forall v . Map v -> Int
size m =
  case m of
    Empty -> 0
    Leaf _ _ -> 1
    Node2 _ _ m1 m2 -> size m1 + size m2
    Node3 _ _ m1 m2 m3 -> size m1 + size m2 + size m3
