--
-- Balanced binary trees
-- Similar to Data.Map
-- Based on https://ufal.mff.cuni.cz/~straka/papers/2011-bbtree.pdf
--
module Data.Map(
  Map,
  insertBy, insertByWith, fromListByWith, fromListBy, lookupBy, empty, elems, size, toList, deleteBy,
  ) where
import Prelude()              -- do not import Prelude
import Prelude hiding (lookupBy, deleteBy)

data Map k a
  = Nil           -- empty tree
  | One k a       -- singleton
  | Node          -- tree node
    (Map k a)      -- left subtree
    Int            -- size of this tree
    k              -- key stored in the node
    a              -- element stored in the node
    (Map k a)      -- right subtree
  --Xderiving(Show)

empty :: forall k a . Map k a
empty = Nil

elems :: forall k v . Map k v -> [v]
elems = map snd . toList

toList :: forall k v . Map k v -> [(k, v)]
toList t = to t []
  where
    to Nil q = q
    to (One k v) q = (k, v):q
    to (Node l _ k v r) q = to l ((k, v) : to r q)

fromListBy :: forall k v . (k -> k -> Ordering) -> [(k, v)] -> Map k v
fromListBy cmp = fromListByWith cmp const

fromListByWith :: forall k v . (k -> k -> Ordering) -> (v -> v -> v) -> [(k, v)] -> Map k v
fromListByWith cmp comb = foldr (uncurry (insertByWith cmp comb)) empty

size :: forall k a . Map k a -> Int
size Nil = 0
size (One _ _) = 1
size (Node _ s _ _ _) = s

node :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
node Nil  key val Nil   = One key val
node left key val right = Node left (size left + 1 + size right) key val right

lookupBy :: forall k a . (k -> k -> Ordering) -> k -> Map k a -> Maybe a
lookupBy cmp k = look
  where
    look Nil = Nothing
    look (One key val) | isEQ (cmp k key) = Just val
                       | otherwise = Nothing
    look (Node left _ key val right) =
      case k `cmp` key of
        LT -> look left
        EQ -> Just val
        GT -> look right

insertBy :: forall k a . (k -> k -> Ordering) -> k -> a -> Map k a -> Map k a
insertBy cmp = insertByWith cmp const

insertByWith :: forall k a . (k -> k -> Ordering) -> (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertByWith cmp comb k v = ins
  where
    ins Nil = One k v
    ins (One a v) = ins (Node Nil 1 a v Nil)
    ins (Node left _ key val right) =
      case k `cmp` key of
        LT -> balance (ins left) key val right
        EQ -> node left k (comb v val) right
        GT -> balance left key val (ins right)

deleteBy :: forall k a . (k -> k -> Ordering) -> k -> Map k a -> Map k a
deleteBy cmp k = del
  where
    del Nil = Nil
    del t@(One a _) | isEQ (k `cmp` a) = Nil
                    | otherwise        = t
    del (Node left _ key val right) =
      case k `cmp` key of
        LT -> balance (del left) key val right
        EQ -> glue left right
        GT -> balance left key val (del right)
      where
        glue Nil right = right
        glue left Nil = left
        glue left right
          | size left > size right =
            let (key', val', left') = extractMax left
            in node left' key' val' right
          | otherwise =
            let (key', val', right') = extractMin right
            in node left key' val' right'
extractMin :: forall k a . Map k a -> (k, a, Map k a)
extractMin Nil = undefined
extractMin (One key val) = (key, val, Nil)
extractMin (Node Nil _ key val right) = (key, val, right)
extractMin (Node left _ key val right) =
  case extractMin left of
    (min, vmin, left') -> (min, vmin, balance left' key val right)

extractMax :: forall k a . Map k a -> (k, a, Map k a)
extractMax Nil = undefined
extractMax (One key val) = (key, val, Nil)
extractMax (Node left _ key val Nil) = (key, val, left)
extractMax (Node left _ key val right) =
  case extractMax right of
    (max, vmax, right') -> (max, vmax, balance left key val right')

omega :: Int
omega = 3
alpha :: Int
alpha = 2
delta :: Int
delta = 0

balance :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
balance left key val right
  | size left + size right <= 1 = node left key val right
balance (One k v) key val right = balance (Node Nil 1 k v Nil) key val right
balance left key val (One k v)  = balance left key val (Node Nil 1 k v Nil)
balance left key val right
  | size right > omega * size left + delta =
      case right of
        (Node rl _ _ _ rr) | size rl < alpha*size rr -> singleL left key val right
                           | otherwise -> doubleL left key val right
        _ -> undefined
  | size left > omega * size right + delta =
      case left of
        (Node ll _ _ _ lr) | size lr < alpha*size ll -> singleR left key val right
                           | otherwise -> doubleR left key val right
        _ -> undefined
  | otherwise = node left key val right

singleL :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
singleL l k v (Node rl _ rk rv rr) = node (node l k v rl) rk rv rr
singleL _ _ _ _ = undefined

singleR :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
singleR (Node ll _ lk lv lr) k v r = node ll lk lv (node lr k v r)
singleR _ _ _ _ = undefined

doubleL :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
doubleL l k v (Node (Node rll _ rlk rlv rlr) _ rk rv rr) = node (node l k v rll) rlk rlv (node rlr rk rv rr)
doubleL l k v (Node (One        rlk rlv    ) _ rk rv rr) = node (node l k v Nil) rlk rlv (node Nil rk rv rr)
doubleL _ _ _ _ = undefined

doubleR :: forall k a . Map k a -> k -> a -> Map k a -> Map k a
doubleR (Node ll _ lk lv (Node lrl _ lrk lrv lrr)) k v r = node (node ll lk lv lrl) lrk lrv (node lrr k v r)
doubleR (Node ll _ lk lv (One        lrk lrv    )) k v r = node (node ll lk lv Nil) lrk lrv (node Nil k v r)
doubleR _ _ _ _ = undefined

isEQ :: Ordering -> Bool
isEQ EQ = True
isEQ _  = False
