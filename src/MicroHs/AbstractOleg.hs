{-# LANGUAGE PatternSynonyms #-}
module MicroHs.AbstractOleg(compileOpt) where
-- Oleg's abstraction algorithm
import Prelude
import MicroHs.Exp
import MicroHs.Expr(Lit(..))
import MicroHs.Ident

first :: forall a b c . (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

data Peano = S Peano | Z
  deriving (Show)
data DB = N Peano | L DB | A DB DB | Free Ident | K Lit
  deriving (Show)

index :: Ident -> [Ident] -> Maybe Peano
index x xs = lookup x $ zip xs $ iterate S Z

deBruijn :: Exp -> DB
deBruijn = go [] where
  go binds e =
    case e of
      Var x -> maybe (Free x) N $ index x binds
      App t u -> A (go binds t) (go binds u)
      Lam x t -> L $ go (x:binds) t
      Lit l -> K l

type CL = Exp
type BCL = ([Bool], CL)

com :: String -> Exp
com s = Lit (LPrim s)
(@@) :: Exp -> Exp -> Exp
(@@) f a = App f a

convertBool :: (BCL -> BCL -> CL) -> DB -> BCL
convertBool (#) ee =
  case ee of
    N Z -> (True:[], com "I")
    N (S e) -> (False:g, d) where (g, d) = rec $ N e
    L e -> case rec e of
             ([], d) -> ([], com "K" @@ d)
             (False:g, d) -> (g, ([], com "K") # (g, d))
             (True:g, d) -> (g, d)
    A e1 e2 -> (zipWithDefault False (||) g1 g2, t1 # t2) where
      t1@(g1, _) = rec e1
      t2@(g2, _) = rec e2
    Free s -> ([], Var s)
    K l -> ([], Lit l)
  where rec = convertBool (#)

convert :: ((Int, Exp) -> (Int, Exp) -> Exp) -> DB -> (Int, Exp)
convert (#) ee =
  case ee of
    N Z -> (1, com "I")
    N (S e) -> (n + 1, (0, com "K") # t) where t@(n, _) = rec $ N e
    L e -> case rec e of
             (0, d) -> (0, com "K" @@ d)
             (n, d) -> (n - 1, d)
    A e1 e2 -> (max n1 n2, t1 # t2) where
      t1@(n1, _) = rec e1
      t2@(n2, _) = rec e2
    K l -> (0, Lit l)
    Free s -> (0, Var s)
  where rec = convert (#)

plain :: DB -> (Int, Exp)
plain = convert (#) where
  (#) (0 , d1) (0 , d2) = d1 @@ d2
  (#) (0 , d1) (n , d2) = (0, com "B" @@ d1) # (n - 1, d2)
  (#) (n , d1) (0 , d2) = (0, com "R" @@ d2) # (n - 1, d1)
  (#) (n1, d1) (n2, d2) = (n1 - 1, (0, com "S") # (n1 - 1, d1)) # (n2 - 1, d2)

optEta :: DB -> BCL
optEta = convertBool (#) where
  (#) ([], d1)           {- # -} ([],       d2)      = d1 @@ d2
  (#) ([], d1)           {- # -} (True:[],  Lit (LPrim "I")) = d1
  (#) ([], d1)           {- # -} (True:g2,  d2)      = ([], com "B" @@ d1) # (g2, d2)
  (#) ([], d1)           {- # -} (False:g2, d2)      = ([], d1) # (g2, d2)
  (#) (True:[], Lit (LPrim "I")) {- # -} ([],       d2)      = com "U" @@ d2
  (#) (True:[], Lit (LPrim "I")) {- # -} (False:g2, d2)      = ([], com "U") # (g2, d2)
  (#) (True:g1, d1)      {- # -} ([],       d2)      = ([], com "R" @@ d2) # (g1, d1)
  (#) (True:g1, d1)      {- # -} (True:g2,  d2)      = (g1, ([], com "S") # (g1, d1)) # (g2, d2)
  (#) (True:g1, d1)      {- # -} (False:g2, d2)      = (g1, ([], com "C") # (g1, d1)) # (g2, d2)
  (#) (False:g1, d1)     {- # -} ([],       d2)      = (g1, d1) # ([], d2)
  (#) (False:_g1, d1)    {- # -} (True:[],  Lit (LPrim "I")) = d1
  (#) (False:g1, d1)     {- # -} (True:g2,  d2)      = (g1, ([], com "B") # (g1, d1)) # (g2, d2)
  (#) (False:g1, d1)     {- # -} (False:g2, d2)      = (g1, d1) # (g2, d2)

zipWithDefault :: forall a b . a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithDefault d f     []     ys = map (f d) ys
zipWithDefault d f     xs     [] = map (flip f d) xs
zipWithDefault d f (x:xt) (y:yt) = f x y : zipWithDefault d f xt yt

bulkPlain :: (String -> Int -> Exp) -> DB -> (Int, Exp)
bulkPlain blk = convert (#) where
  (#) (a, x) (b, y) = case (a, b) of
    (0, 0)             ->               x @@ y
    (0, n)             -> blk "B" n @@ x @@ y
    (n, 0)             -> blk "C" n @@ x @@ y
    (n, m) | n == m    -> blk "S" n @@ x @@ y
           | n < m     ->                     blk "B" (m - n) @@ (blk "S" n @@ x) @@ y
           | otherwise -> blk "C" (n - m) @@ (blk "B" (n - m) @@  blk "S" m @@ x) @@ y

bulkMax :: Int
bulkMax = 3

bulk :: String -> Int -> Exp
bulk c 1 = com c
bulk c n | n <= bulkMax = com (c ++ show n)
         | otherwise = App (com (c ++ "'")) (bulk c (n - 1))

----------------------------------------

compileOpt :: Exp -> Exp
--compileOpt = optCons . snd . optEta . deBruijn
compileOpt = optCons . snd . bulkOpt bulk . deBruijn

optCons :: Exp -> Exp
-- ((B (B K)) ((B C) U))  -->  O
optCons (App (App (Lit (LPrim "B")) (App (Lit (LPrim "B")) (Lit (LPrim "K")))) (App (App (Lit (LPrim "B")) (Lit (LPrim "C"))) (Lit (LPrim "U")))) = com "O"
-- ((B2 K) ((B C) U))  -->  O
optCons (App (App (Lit (LPrim "B2")) (Lit (LPrim "K"))) (App (App (Lit (LPrim "B")) (Lit (LPrim "C"))) (Lit (LPrim "U")))) = com "O"
-- (((B' B) K) P)
optCons (App (App (App (Lit (LPrim "B'")) (Lit (LPrim "B"))) (Lit (LPrim "K"))) (App (App (Lit (LPrim "B")) (Lit (LPrim "C"))) (Lit (LPrim "U")))) = com "O"
-- ((B C) U)  -->  P
optCons (App (App (Lit (LPrim "B")) (Lit (LPrim "C"))) (Lit (LPrim "U"))) = com "P"
-- (K I)  -->  A
optCons (App (Lit (LPrim "K")) (Lit (LPrim "I"))) = com "A"
-- (B K)  -->  Z
optCons (App (Lit (LPrim "B")) (Lit (LPrim "K"))) = com "Z"
optCons (App f a) = App (optCons f) (optCons a)
optCons e = e

bulkOpt :: (String -> Int -> Exp) -> DB -> ([Bool], Exp)
bulkOpt blk ee =
 case ee of
  N Z -> (True:[], com "I")
  N (S e) -> first (False:) $ rec $ N e
  L e -> case rec e of
    ([], d) -> ([], com "K" @@ d)
    (False:g, d) -> ([], com "K") ## (g, d)
    (True:g, d) -> (g, d)
  A e1 e2 -> rec e1 ## rec e2
  K l -> ([], Lit l)
  Free s -> ([], Var s)
 where
  rec = bulkOpt blk
  (##) ([], d1) ([], d2) = ([], d1 @@ d2)
  (##) ([], d1) ([True], Lit (LPrim "I")) = ([True], d1)
  (##) ([], d1) (g2, Lit (LPrim "I")) | and g2 = (g2, blk "B" (length g2 - 1) @@ d1)
  (##) ([], d1) (g2@(h:_), d2) = first (pre++) $ ([], fun1 d1) ## (post, d2)
    where
    fun1 = case h of
      True -> (blk "B" (length pre) @@)
      False -> id
    (pre, post) = span (h ==) g2

  (##) ([True], Lit (LPrim "I")) ([], d2) = ([True], com "U" @@ d2)
  (##) (g1@(h:_), d1) ([], d2) = first (pre++) $ case h of
    True -> ([], com "C" @@ blk "C" (length pre) @@ d2) ## (post, d1)
    False -> (post, d1) ## ([], d2)
   where
    (pre, post) = span (h ==) g1

  (##) ([True], Lit (LPrim "I")) (False:g2, d2) = first (True:) $ ([], com "U") ## (g2, d2)
  (##) (False:g1, d1) ([True], Lit (LPrim "I")) = (True:g1, d1)
  (##) (g1, d1) (g2, Lit (LPrim "I")) | and g2, let { n = length g2 }, all not $ take n g1 =
    first (g2++) $ ([], blk "B" $ n - 1) ## (drop n g1, d1)
  (##) (g1, d1) (g2, d2) = pre $ fun1 (drop count g1, d1) ## (drop count g2, d2)
    where
    (h, count) = headGroup $ zip g1 g2
    fun1 = case h of
      (False, False) -> id
      (False, True) -> apply "B"
      (True, False) -> apply "C"
      (True, True) -> apply "S"
    pre = first (replicate count (uncurry (||) h) ++)
    apply s = (([], blk s count) ##)

headGroup :: forall a . Eq a => [a] -> (a, Int)
headGroup (h:t) = (h, 1 + length (takeWhile (== h) t))
headGroup _ = undefined

---
-- K (K (K foo))
-- K (K (K foo)) x y z -->
--    K (K foo)    y z -->
--       K foo       z -->
--       foo
--
-- B (B (B foo))  x y  z  w -->
--    B (B foo)  (x y) z  w -->
--      (B foo)  (x y  z) w -->
--         foo   (x y  z  w)
--
-- Z (Z (Z foo))  x y z w   -->
--    Z (Z foo)   x   z w   -->
--       Z foo    x     w   -->
--         foo    x
-- Z (Z foo) x y z -->
--    Z foo  x   z -->
--      foo  x
-- Z2 x y z w = x y
--
-- U K x y  -->  x K y z
-- B U x y z  -->  U (x y) z --> z (x y)
-- C' S x y z w  -->  S (x y) z w --> x y w (z w)
