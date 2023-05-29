{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Exp where
--import Data.List
import Parse(Ident)

pp :: Exp -> IO ()
pp = putStrLn . toString

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
    (App CB CC,              App CC CI) -> Comb "P"
    (App CB (App CB CK),     Comb "P")  -> Comb "O"
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
