module Loads.ExprEval(main) where

-- A tiny expression AST, generated as a large, deterministic, roughly
-- balanced tree and then evaluated. Stresses tree-shaped allocation and
-- non-tail-recursive pattern matching.
data Expr
  = Lit !Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

build :: Int -> Int -> Expr
build 0 seed = Lit (seed `rem` 17 - 8)
build d seed =
  case seed `rem` (3 :: Int) of
    0 -> Add (build (d - 1) (seed * 2 + 1)) (build (d - 1) (seed * 2 + 2))
    1 -> Sub (build (d - 1) (seed * 2 + 1)) (build (d - 1) (seed * 2 + 2))
    _ -> Mul (build (d - 1) (seed * 2 + 1)) (build (d - 1) (seed * 2 + 2))

-- we do mod m to not overflow, which is an error in mhs
m :: Int
m = 1000003

eval :: Expr -> Int
eval (Lit n)   = n `mod` m
eval (Add a b) = (eval a + eval b) `mod` m
eval (Sub a b) = (eval a - eval b) `mod` m
eval (Mul a b) = (eval a * eval b) `mod` m

main :: IO ()
main = do
  let depth = 19
      tree  = build depth 1
  print (eval tree)
