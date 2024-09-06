{-# OPTIONS_GHC -Wall #-}
module Main where
import Control.Monad

infix :@
data Exp
  = K | A | U | I | B | B' | Z | C | C' | P | R | O | K2 | K3 | K4 | C'B
  | Exp :@ Exp | Var String
  deriving (Show, Eq, Ord)

reduce :: Int -> [Exp] -> (Int, [Exp])
reduce n (K : x : _y : xs) = reduce (n+1) (x : xs)
reduce n (A : _x : y : xs) = reduce (n+1) (y : xs)
reduce n (U : x : y : xs) = reduce (n+1) (y : x : xs)
reduce n (I : x : xs) = reduce (n+1) (x : xs)
reduce n (B : x : y : z : xs) = reduce (n+1) (x : (y :@ z) : xs)
reduce n (B' : x : y : z : w : xs) = reduce (n+1) (x : y : (z :@ w) : xs)
reduce n (Z : x : y : _z : xs) = reduce (n+1) (x : y : xs)
reduce n (C : x : y : z : xs) = reduce (n+1) (x : z : y : xs)
reduce n (C' : x : y : z : w : xs) = reduce (n+1) (x : (y :@ w) : z : xs)
reduce n (P : x : y : z : xs) = reduce (n+1) (z : x : y : xs)
reduce n (R : x : y : z : xs) = reduce (n+1) (y : z : x : xs)
reduce n (O : x : y : _z : w : xs) = reduce (n+1) (w : x : y : xs)
reduce n (K2 : x : _y : _z : xs) = reduce (n+1) (x : xs)
reduce n (K3 : x : _y : _z : _w : xs) = reduce (n+1) (x : xs)
reduce n (K4 : x : _y : _z : _w : _v : xs) = reduce (n+1) (x : xs)
reduce n (C'B : x : y : z : w : xs) = reduce (n+1) (x : z : (y :@ w) : xs)
reduce n ((f :@ x) : xs) = reduce (n+1) (f : x : xs)
reduce n xs = (n, xs)

pr :: Exp -> String
pr (e1 :@ e2) = "(" ++ pr e1 ++ " " ++ pr e2 ++ ")"
pr (Var s) = s
pr k = show k

prs :: [Exp] -> String
prs = unwords . map pr

xx,yy,zz :: Exp
xx = Var "x"
yy = Var "y"
zz = Var "z"

t1 :: [Exp]
t1 = [B, xx, yy, zz]

combs :: [Exp]
combs = [K, A, U, I, B, B', Z, C, C', P, R, O, K2, K3, K4, C'B]

vars :: [Exp]
vars = map Var $ ["x","y","z","w","v","u"] ++ ["x" ++ show i | i <- [1::Int ..] ]

rand :: Int -> [[Exp]]
rand n = do
  k <- combs
--  guard (k `notElem` [I, K, A, K2, K3, K4])   -- uninteresting
  xs <- rand' n vars
  return (k:xs)

rand' :: Int -> [Exp] -> [[Exp]]
rand' 0 vs = return $ take 20 vs
rand' n vs =
    do
      k <- combs
      xs <- rand' (n-1) vs
      return (k:xs)
 ++
    do
      let v = vs !! 0
      xs <- rand' (n-1) (drop 1 vs)
      return (v:xs)

red :: [Exp] -> [([Exp], [Exp])]
red xs =
  case reduce 0 xs of
    (1, _) -> []
    (_, ys) -> [dropEqualSuffix xs ys]

dropEqualSuffix :: Eq a => [a] -> [a] -> ([a], [a])
dropEqualSuffix xxs yys = loop (reverse xxs) (reverse yys)
  where loop (x:xs) (y:ys) | x == y && not (null xs) && not (null ys) = loop xs ys
        loop xs ys = (reverse xs, reverse ys)

isApp :: Exp -> Bool
--isApp (_ :@ _) = True
isApp _ = False

main :: IO ()
main = do
--  putStrLn $ prs t1
--  putStrLn $ prs $ snd $ reduce 0 t1
  let ess = rand 2
      rss = concatMap red ess
      rss' = filter (\ (_, es) -> all (not . isApp) es) rss
  mapM_ (\ (a, b) -> putStrLn (prs a ++ "  -->  " ++ prs b)) rss'
