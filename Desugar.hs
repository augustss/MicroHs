module Desugar(
  desugar,
  Module(..),
  Export,
  TypeDef(..),
  LDef,
  SymTable,
  TypeTable,
  ) where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Parse
import Exp

data Module = Module IdentModule [Export] [TypeDef] [LDef]
  deriving (Show)

type Export = (Ident, Ident)  -- exported name, global name

data TypeDef = TypeDef Ident [(Ident, Int)]   -- constructor name, arity
  deriving (Show, Eq)

type LDef = (Ident, Exp)

type SymTable = M.Map Ident [Exp]
type TypeTable = M.Map Ident [TypeDef]

desugar :: [(ImportSpec, Module)] -> EModule -> Module
desugar imdls (EModule mdln especs ds) =
  let ds' = concatMap (dsDef allSyms allTypes) ds
      tyds = concatMap dsData ds
      exps = concatMap export especs
      export (ExpModule m) =
        if m == mdln then
          [(i, qual mdln i) | (i, _) <- ds']
        else
          [ e | (_, Module mn es _ _) <- imdls, mn == m, e <- es ]
      mdl = Module mdln exps tyds [(qual mdln i, e) | (i, e) <- ds']
      mdls = (ImportSpec False mdln Nothing, mdl) : imdls
      qns (ImportSpec q _ mas) mn i =
        let mn' = fromMaybe mn mas
        in  if q then [qual mn' i] else [i, qual mn' i]
      allSyms :: SymTable
      allSyms = M.fromListWith union $ concatMap syms mdls
        where syms (is, Module mn qis _ _) = [ (v, [Var qi]) | (i, qi) <- qis, v <- qns is mn i ]
      allTypes :: TypeTable
      allTypes = M.fromListWith union $ concatMap types mdls
        where types (is, Module mn _ tds _) = [ (v, [td]) | td@(TypeDef _ cs) <- tds, (c, _) <- cs, v <- qns is mn c ]
  in  mdl

dsDef :: SymTable -> TypeTable -> EDef -> [LDef]
dsDef _ _ (Data _ cs) = zipWith dsConstr [0..] cs
  where
    fs = [f i | (i, _) <- zip [0..] cs]
    dsConstr i (c, ts) = (c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      where xs = ["$x" ++ show (j::Int) | (j, _) <- zip [0..] ts]
    f i = "$f" ++ show (i::Int)
dsDef syms tys (Fcn (f, xs) e) = [(f, lams xs $ dsExpr (extSyms syms xs) tys e)]
dsDef _ _ Sign{} = []
dsDef _ _ Import{} = []

dsExpr :: SymTable -> TypeTable -> Expr -> Exp
dsExpr syms _ (EVar i) =
  case M.lookup i syms of
    Nothing -> error $ "undefined: " ++ show i
    Just [qi] -> qi
    Just qis -> error $ "ambiguous: " ++ show i ++ ", " ++ show qis
dsExpr syms tys (EApp f a) = App (dsExpr syms tys f) (dsExpr syms tys a)
dsExpr syms tys (ELam xs e) = lams xs (dsExpr (extSyms syms xs) tys e)
dsExpr _ _ (EInt i) = Int i
dsExpr _ _ (EChar c) = Chr c
dsExpr syms tys (ECase e as) = apps (dsExpr syms tys e) (map dsArm as')
  where dsArm (PConstr _ vs, r) = lams vs $ dsExpr (extSyms syms vs) tys r
        dsArm (PTuple [_], _) = error "dsExpr: singleton tuple"
        dsArm (PTuple vs, r) = lams vs $ dsExpr (extSyms syms vs) tys r
        as' = reorderArms tys as
-- For now, just sequential bindings; each recursive
dsExpr syms tys (ELet [] e) = dsExpr syms tys e
dsExpr syms tys (ELet (d:ds) e) =
  let ds' = dsDef syms' tys d
      syms' = extSyms syms (map fst ds')
      e' = dsExpr syms' tys (ELet ds e)
      def (i, r) a = App (Lam i a) (App (Prim "Y") (Lam i r))
  in  foldr def e' ds'
dsExpr syms tys (EList es) =
  foldr (App2 CO) CK $ map (dsExpr syms tys) es
dsExpr _ _ (ETuple []) = Lam "_x" (Var "_x")    -- encoding of ()
dsExpr syms tys (ETuple [e]) = dsExpr syms tys e
dsExpr syms tys (ETuple es) = Lam "_f" $ foldl App (Var "_f") $ map (dsExpr syms tys) es
dsExpr syms tys (EStr cs) = dsExpr syms tys $ EList $ map EChar cs
dsExpr _ _ (EDo _ []) = error "empty do"
dsExpr _ _ (EDo _ [Bind _ _]) = error "do without final expression"
dsExpr syms tys (EDo _ [Then e]) = dsExpr syms tys e
dsExpr syms tys (EDo mn (Bind i e : ss)) =
  dsExpr syms tys $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam [i] (EDo mn ss))
dsExpr syms tys (EDo mn (Then   e : ss)) =
  dsExpr syms tys $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn ss)
dsExpr syms tys (EDo mn (Let i e : ss)) =
  dsExpr syms tys $ ELet [Fcn (i, []) e] (EDo mn ss)
dsExpr _ _ (EPrim s) = Prim s

mqual :: Maybe Ident -> Ident -> Ident
mqual (Just qi) i = qual qi i
mqual Nothing   i = i

reorderArms :: TypeTable -> [(EPat, Expr)] -> [(EPat, Expr)]
reorderArms _ [] = error "case has no arms"
reorderArms _ as@[(PTuple _, _)] = as
reorderArms tys as@((PConstr con _, _) : _) =
  let arms = [(c, a) | a@(PConstr c _, _) <- as] in
  if length arms /= length as then error "bad tuple pattern" else
  case M.lookup con tys of
    Nothing -> error $ "undefined constructor: " ++ show con
    Just [TypeDef _ cs] -> map arm cs
      where arm (c, k) =
              case lookup c arms of
                Nothing -> error $ "constructor missing: " ++ show (c, con)
                Just a@(PConstr _ vs, _) ->
                  if length vs == k then a else error $ "bad contructor arity: " ++ show a
                _ -> undefined
    Just _tds -> error $ "ambiguous constructor: " ++ show con
reorderArms _ _ = undefined

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

dsData :: EDef -> [TypeDef]
dsData (Data (tn, _) cs) = [TypeDef tn [(c, length ts) | (c, ts) <- cs ]]
dsData _ = []

extSyms :: SymTable -> [Ident] -> SymTable
extSyms = foldr (\ x -> M.insert x [Var x])
