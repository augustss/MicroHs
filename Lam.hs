{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
import Data.List
type Ident = String

main :: IO ()
main = do
{-
  let c1 = compile e1
      c1' = improve c1
      r1' = reduce (App c1 (Var "k"))
      r1'' = reduce (App c1 (Int 5))
  pp e1
  pp c1
  pp c1'
  pp r1'
  pp r1''
  pp $ reduce (App c1' (Int 6))
  let c2 = compile e2
      c2' = improve c2
  pp e2
  pp c2
  pp c2'
  pp $ reduce $ App2 c2 (Int 2) (Int 10)
  pp $ App2 c2' (Int 2) (Int 10)
  pp $ reduce $ App2 c2' (Int 2) (Int 10)
  pp $ reduce $ App2 c2' (Var "x") (Var "y")
  pp $ improve $ compile e3
  pp $ improve $ compile $ App (Lam "x" (App2 (Var "x") (Int 10) (Int 2))) (Prim "-")
-}
  putStrLn $ toStringP $ eNfib `App` 35

pp :: Exp -> IO ()
pp = putStrLn . toString

e1 :: Exp
e1 = Lam "x" $ App2 (Prim "+") (Var "x") (Var "x")
e2 :: Exp
e2 = Lam "x" $ Lam "y" $ App2 (Prim "-") (Var "y") (Var "x")
e3 :: Exp
e3 = Lam "x" $ Lam "y" $ Var "y"


data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Int Integer
  | Comb String
  | Prim String
  deriving (Show, Eq)

pattern App2 :: Exp -> Exp -> Exp -> Exp
pattern App2 x y z = App (App x y) z

pattern App3 :: Exp -> Exp -> Exp -> Exp -> Exp
pattern App3 x y z w = App (App (App x y) z) w

pattern App4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
pattern App4 x y z w v = App (App (App (App x y) z) w) v

pattern CI, CK, CS, CC, CB, CS', CC', CB' :: Exp
pattern CI = Comb "I"
pattern CK = Comb "K"
pattern CS = Comb "S"
pattern CC = Comb "C"
pattern CB = Comb "B"
pattern CS' = Comb "S'"
pattern CC' = Comb "C'"
pattern CB' = Comb "B'"
pattern CT = Comb "T"
pattern CY = Comb "Y"

toStringP :: Exp -> String
toStringP (Var x) = x
toStringP (Prim x) = x
toStringP (Comb x) = x
toStringP (Int i) = show i
toStringP (Lam x e) = "(\\" ++ x ++ " " ++ toStringP e ++ ")"
toStringP (App f a) = "(" ++ toStringP f ++ " " ++ toStringP a ++ ")"

toString :: Exp -> String
toString (Var x) = x
toString (Prim x) = x
toString (Comb x) = x
toString (Int i) = show i
toString (Lam x e) = "(\\" ++ x ++ ". " ++ toString e ++ ")"
toString (App f a) = toString f ++ " " ++ par a
  where par e@App{} = "(" ++ toString e ++ ")"
        par e = toString e

compile :: Exp -> Exp
compile (App f a) = App (compile f) (compile a)
compile (Lam x a) = abstract x a
compile e = e

abstract :: Ident -> Exp -> Exp
abstract x e@(Var y) | x == y = CI
abstract x (App f a) = App2 CS (abstract x f) (abstract x a)
abstract x (Lam y e) = abstract x $ improveC $ abstract y e
abstract x e = App CK e

improve :: Exp -> Exp
improve = improveT . improveC

improveC :: Exp -> Exp
improveC (App f a) =
  case (improveC f, improveC a) of
    (App CS (App CK e1),     App CK e2) -> improve (App CK (App e1 e2))
    (App CS (App CK e1),     CI)        -> e1
    (App CS (App CK e1),     e2)        -> improve (App2 CB e1 e2)
    (App CS         e1,      App CK e2) -> improve (App2 CC e1 e2)
    (App CS (App2 CB e1 e2), e3)        -> improve (App3 CS' e1 e2 e3)
    (App CC (App2 CB e1 e2), e3)        -> improve (App3 CC' e1 e2 e3)
--    (CK,                     CI)        -> CT
    -- flip arguments to commutative ops
    (CC,                     Prim "+")  -> Prim "+"
    (CC,                     Prim "*")  -> Prim "*"
    (CC,                     Prim "=")  -> Prim "="
    (CC,                     Prim "!=") -> Prim "!="
    -- flip arguments when a flipped version exists
    (CC,                     Prim "-")  -> Prim "-'"
    (CC,                     Prim "<")  -> Prim ">"
    (CC,                     Prim ">")  -> Prim "<"
    (CC,                     Prim "<=") -> Prim ">="
    (CC,                     Prim ">=") -> Prim "<="
    (e1,                     e2)        -> App e1 e2
improveC e = e

improveT :: Exp -> Exp
improveT (App f a) =
  case (improve f, improve a) of
    (CK,                     CI)        -> CT
    (e1,                     e2)        -> App e1 e2
improveT e = e

reduce :: Exp -> Exp
reduce e =
  case redOne e of
    Just e' -> reduce e'
    Nothing ->
      case e of
        App f a | f /= f' -> reduce (App f' a)
          where f' = reduce f
        _ -> e

redOne :: Exp -> Maybe Exp
redOne (App CI x) = Just x
redOne (App (App CK x) _) = Just x
redOne (App (App CT _) x) = Just x
redOne (App3 CS f g x) = Just $ App (App f x) (App g x)
redOne (App3 CB f g x) = Just $ App      f    (App g x)
redOne (App3 CC f g x) = Just $ App (App f x)      g
redOne (App4 CS' k f g x) = Just $ App2 k (App f x) (App g x)
redOne (App4 CB' k f g x) = Just $ App2 k      f    (App g x)
redOne (App4 CC' k f g x) = Just $ App2 k (App f x)      g
redOne e@(App2 (Prim p) x y) | Just op <- lookup p binOps,
                               let e' = op p (reduce x) (reduce y),
                               e' /= e = Just e'
--redOne (Var i) = error $ "Var " ++ show i
redOne e = Nothing

binOps :: [(String, String -> Exp -> Exp -> Exp)]
binOps = [
  ("+", arith (+)),
  ("-", arith (-)),
  ("*", arith (*)),
  ("div", arith div)
  ]
  where
    arith f s (Int x) (Int y) = Int (x `f` y)
    arith _ s x y = App2 (Prim s) x y
--    arith _ x y = error $ "arith: " ++ show (x, y)
    

-------------------

instance Num Exp where
  x + y = App2 (Prim "+") x y
  x - y = App2 (Prim "-") x y
  x * y = App2 (Prim "*") x y
  fromInteger = Int
  signum x = iF (x .< 0) (-1) (iF (0 .< x) 1 0)
  abs x = iF (x .< 0) (-x) x

iF :: Exp -> Exp -> Exp -> Exp
iF c t e = App2 c e t

(.==), (.<) :: Exp -> Exp -> Exp
x .== y = App2 (Prim "=") x y
x .< y = App2 (Prim "<") x y

fix :: Exp -> Exp
fix e = App CY e

allVars :: Exp -> [Ident]
allVars (Var x) = [x]
allVars (App f a) = union (allVars f) (allVars a)
allVars (Lam x e) = union [x] (allVars e)
allVars _ = []

class ToLam e where
  toLam :: e -> Exp

instance ToLam Exp where
  toLam e = e

instance (ToLam e, a ~ Exp) => ToLam (a -> e) where
  toLam f =
    let vs = allVars $ toLam $ f (Int 0)
        v = head $ (["x","y","z"] ++ ["v" ++ show i | i <- [1..]]) \\ vs
    in  Lam v $ toLam $ f (Var v)

infixl 9 $$
($$) :: (ToLam e1, ToLam e2) => e1 -> e2 -> Exp
e1 $$ e2 = App (toLam e1) (toLam e2)

comp :: (ToLam e) => e -> Exp
comp = improve . compile . toLam

eFac = fix $ comp $ \ fac n -> iF (n .== 0) 1 (n * fac $$ (n - 1))

yC = comp $ \ f -> (\ x -> f $$ (x $$ x)) $$ (\ x -> f $$ (x $$ x))

eNfib = fix $ comp $ \ nfib n -> iF (n .< 2) 1 (nfib $$ (n-1) + nfib $$ (n-2) + 1)

tst = App CC' CS
