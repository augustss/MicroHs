{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Exp where
import Parse(Ident)
--import Debug.Trace

type PrimOp = String

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Int Integer
  | Chr Char
  | Prim PrimOp
  | Lbl Int Exp
  deriving (Show, Eq)

pattern App2 :: Exp -> Exp -> Exp -> Exp
pattern App2 x y z = App (App x y) z

pattern App3 :: Exp -> Exp -> Exp -> Exp -> Exp
pattern App3 x y z w = App (App (App x y) z) w

pattern App4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
pattern App4 x y z w v = App (App (App (App x y) z) w) v

pattern CI, CK, CS, CC, CB, CS', CC', CB', CT, CY, CP, CO :: Exp
pattern CI = Prim "I"
pattern CK = Prim "K"
pattern CS = Prim "S"
pattern CC = Prim "C"
pattern CB = Prim "B"
pattern CS' = Prim "S'"
pattern CC' = Prim "C'"
pattern CB' = Prim "B'"
pattern CT = Prim "T"
pattern CY = Prim "Y"
pattern CP = Prim "P"
pattern CO = Prim "O"

toStringP :: Exp -> String
toStringP (Var x) = x
toStringP (Prim x) = '$':x
toStringP (Int i) = show i
toStringP (Chr c) = ['\'', c]
toStringP (Lam x e) = "(\\" ++ x ++ " " ++ toStringP e ++ ")"
toStringP (App f a) = "(" ++ toStringP f ++ " " ++ toStringP a ++ ")"
toStringP (Lbl i e) = ":" ++ show i ++ " " ++ toStringP e

compileOpt :: Exp -> Exp
compileOpt = improveT . compile

compile :: Exp -> Exp
compile (App f a) = App (compile f) (compile a)
compile (Lam x a) = abstract x a
compile e = e

abstract :: Ident -> Exp -> Exp
abstract x (Var y) | x == y = CI
abstract x (App f a) = cS (abstract x f) (abstract x a)
abstract x (Lam y e) = abstract x $ abstract y e
abstract _ e = cK e

cK :: Exp -> Exp
--cK CI = CT
cK e  = App CK e

cS :: Exp -> Exp -> Exp
--cS e1 e2 | trace ("S (" ++ toString e1 ++ ") (" ++ toString e2 ++ ")") False = undefined
cS (App CK e1)     (App CK e2) = cK (App e1 e2)    -- S (K e1) (K e2) = K (e1 e2)
cS (App CK e1)     CI          = e1                -- S (K e1) I      = e1
cS (App CK e1)     e2          = cB e1 e2          -- S (K e1) e2     = B e1 e2
cS e1              (App CK e2) = cC e1 e2          -- S e1     (K e2) = C e1 e2
cS (App2 CB e1 e2) e3          = cS' e1 e2 e3      -- S (B e1 e2) e3  = S' e1 e2 e3
cS e1 e2                       = App2 CS e1 e2

cC :: Exp -> Exp -> Exp
cC (App2 CB e1 e2) e3          = cC' e1 e2 e3      -- C (B e1 e2) e3  = C' e1 e2 e3
cC (Var op)        e2 | Just op' <- lookup op flipOps = App (Var op') e2 -- C op e = flip-op e
cC (App2 CC CI e1) e2          = App2 CP e1 e2
cC e1              e2          = App2 CC e1 e2

cB :: Exp -> Exp -> Exp
cB CC          (App CC CI)    = CP -- Pair
cB (App CB CK) CP             = CO -- Cons
cB CY          (App2 CB CK e) = e  -- B Y (B K e) = e
cB CI          e              = e  -- B I e = e
cB e1          e2             = App2 CB e1 e2

cS' :: Exp -> Exp -> Exp -> Exp
cS' e1 e2 e3 = App3 CS' e1 e2 e3

cC' :: Exp -> Exp -> Exp -> Exp
cC' e1 e2 e3 = App3 CC' e1 e2 e3

-- This is a hack, it assumes things about the Prelude
flipOps :: [(PrimOp, PrimOp)]
flipOps =
  [("Prelude.+",  "Prelude.+")
  ,("Prelude.-",  "Prelude.subtract")
  ,("Prelude.*",  "Prelude.*")
  ,("Prelude.==", "Prelude.==")
  ,("Prelude./=", "Prelude./=")
  ,("Prelude.<",  "Prelude.>")
  ,("Prelude.<=", "Prelude.>=")
  ,("Prelude.>",  "Prelude.<")
  ,("Prelude.>=", "Prelude.<=")
  ]

improveT :: Exp -> Exp
improveT (App f a) =
  case (improveT f, improveT a) of
    (CK,                     CI) -> CT
    (CY,               App CK e) -> e
    (e1,                     e2) -> App e1 e2
improveT e = e
