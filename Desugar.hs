module Desugar(desugar, LDef) where
import Parse
import Exp

type LDef = (Ident, Exp)

desugar :: Module -> (Ident, [LDef])
desugar (Module nm ds) = (nm, concatMap dsDef ds)

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f xs = foldl App f xs

dsDef :: Def -> [LDef]
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
dsExpr (ELam x e) = Lam x (dsExpr e)
dsExpr (EInt i) = Int i
dsExpr (EChar c) = Chr c
dsExpr (ECase e as) = apps (dsExpr e) (map dsArm as)
  where dsArm (PConstr _ vs, r) = lams vs $ dsExpr r
        dsArm (PTuple [_], _) = error "dsExpr: singleton tuple"
        dsArm (PTuple vs, r) = lams vs $ dsExpr r
-- For now, just sequential bindings.
dsExpr (ELet ds e) =
  let ds' = concatMap dsDef ds
      e' = dsExpr e
      def (i, d) a = App (Lam i a) d
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
