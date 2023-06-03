module Desugar(
  desugar,
  Module(..),
  Export,
  TypeDef(..),
  LDef,
  ) where
import Parse
import Exp

data Module = Module IdentModule [Export] [TypeDef] [LDef]
  deriving (Show)

type Export = (Ident, Ident)  -- exported name, global name

data TypeDef = TypeDef Ident [(Ident, Int)]   -- constructor name, arity
  deriving (Show)

type LDef = (Ident, Exp)

desugar :: EModule -> Module
desugar (EModule mn ds) =
  let ds' = concatMap dsDef ds
      tds = concatMap dsData ds
      es = [(i, qual mn i) | (i, _) <- ds']
  in  Module mn es tds [(qual mn i, e) | (i, e) <- ds']

dsDef :: EDef -> [LDef]
dsDef (Data _ cs) = zipWith dsConstr [0..] cs
  where
    fs = [f i | (i, _) <- zip [0..] cs]
    dsConstr i (c, ts) = (c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      where xs = ["$x" ++ show (j::Int) | (j, _) <- zip [0..] ts]
    f i = "$f" ++ show (i::Int)
dsDef (Fcn (f, xs) e) = [(f, lams xs $ dsExpr e)]
dsDef (Sign _ _) = []
dsDef (Import _) = []

dsExpr :: Expr -> Exp
dsExpr (EVar i) = Var i
dsExpr (EApp f a) = App (dsExpr f) (dsExpr a)
dsExpr (ELam xs e) = foldr Lam (dsExpr e) xs
dsExpr (EInt i) = Int i
dsExpr (EChar c) = Chr c
dsExpr (ECase e as) = apps (dsExpr e) (map dsArm as)
  where dsArm (PConstr _ vs, r) = lams vs $ dsExpr r
        dsArm (PTuple [_], _) = error "dsExpr: singleton tuple"
        dsArm (PTuple vs, r) = lams vs $ dsExpr r
-- For now, just sequential bindings; each recursive
dsExpr (ELet ds e) =
  let ds' = concatMap dsDef ds
      e' = dsExpr e
      def (i, d) a = App (Lam i a) (App (Prim "Y") (Lam i d))
  in  foldr def e' ds'
dsExpr (EList es) =
  foldr (App2 CO) CK $ map dsExpr es
dsExpr (ETuple []) = Lam "_x" (Var "_x")    -- encoding of ()
dsExpr (ETuple [e]) = dsExpr e
dsExpr (ETuple es) = Lam "_f" $ foldl App (Var "_f") $ map dsExpr es
dsExpr (EStr cs) = dsExpr $ EList $ map EChar cs
dsExpr (EDo _ []) = error "empty do"
dsExpr (EDo _ [Bind _ _]) = error "do without final expression"
dsExpr (EDo _ [Then e]) = dsExpr e
dsExpr (EDo n (Bind i e : ss)) = App2 (Var (n ++ ".>>=")) (dsExpr e) (Lam i $ dsExpr (EDo n ss))
dsExpr (EDo n (Then   e : ss)) = App2 (Var (n ++ ".>>"))  (dsExpr e)         (dsExpr (EDo n ss))
dsExpr (EDo n (Let  i e : ss)) = App  (Lam i $ dsExpr (EDo n ss)) (dsExpr e)
dsExpr (EPrim s) = Prim s

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f xs = foldl App f xs

dsData :: EDef -> [TypeDef]
dsData (Data (tn, _) cs) = [TypeDef tn [(c, length ts) | (c, ts) <- cs ]]
dsData _ = []
