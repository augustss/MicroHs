{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE PatternSynonyms #-}
-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Exp(
  Exp(..),
  PrimOp,
  substExp,
  app2, app3, cFlip,
  allVarsExp, freeVars,
  lams, apps,
  ) where
import Prelude hiding((<>))
import Data.Char
import Data.List
import MicroHs.Ident
import MicroHs.Expr(Lit(..), showLit)
import Text.PrettyPrint.HughesPJ
import Control.DeepSeq
import Compat
import Debug.Trace

type PrimOp = String

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Lit Lit
  deriving (Eq)

{-
instance Eq Exp where
  (==) (Var i1)    (Var i2)    = i1 == i2
  (==) (App f1 a1) (App f2 a2) = f1 == f2 && a1 == a2
  (==) (Lam i1 e1) (Lam i2 e2) = i1 == i2 && e1 == e2
  (==) (Lit l1)    (Lit l2)    = l1 == l2
  (==) _           _           = False
-}

app2 :: Exp -> Exp -> Exp -> Exp
app2 f a1 a2 = App (App f a1) a2

app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f a1 a2 a3 = App (app2 f a1 a2) a3

cFlip :: Exp
cFlip = Lit (LPrim "C")

--cR :: Exp
--cR = Lit (LPrim "R")

instance Show Exp where
  show = render . ppExp

ppExp :: Exp -> Doc
ppExp ae =
  case ae of
--    Let i e b -> sep [ text "let" <+> ppIdent i <+> text "=" <+> ppExp e, text "in" <+> ppExp b ]
    Var i -> ppIdent i
    App f a -> parens $ ppExp f <+> ppExp a
    Lam i e -> parens $ text "\\" <> ppIdent i <> text "." <+> ppExp e
    Lit l -> text (showLit l)

substExp :: Ident -> Exp -> Exp -> Exp
substExp si se ae =
  case ae of
    Var i -> if i == si then se else ae
    App f a -> App (substExp si se f) (substExp si se a)
    Lam i e -> if si == i then
                 ae
               else if elem i (freeVars se) then
                 let
                   fe = allVarsExp e
                   ase = allVarsExp se
                   j = head [ v | n <- enumFrom (0::Int), let { v = mkIdent ("a" ++ show n) }, not (elem v ase), not (elem v fe) ]
                 in
                   --trace ("substExp " ++ show [si, i, j]) $
                   Lam j (substExp si se (substExp i (Var j) e))
               else
                   Lam i (substExp si se e)
    Lit _ -> ae

-- This naive freeVars seems to be the fastest.
freeVars :: Exp -> [Ident]
freeVars ae =
  case ae of
    Var i -> [i]
    App f a -> freeVars f ++ freeVars a
    Lam i e -> deleteAllBy (==) i (freeVars e)
    Lit _ -> []

allVarsExp :: Exp -> [Ident]
allVarsExp ae =
  case ae of
    Var i -> [i]
    App f a -> allVarsExp f ++ allVarsExp a
    Lam i e -> i : allVarsExp e
    Lit _ -> []

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

