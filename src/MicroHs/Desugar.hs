-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
module MicroHs.Desugar(
  module MicroHs.Desugar
  --desugar, LDef, showLDefs,
  ) where
--import Debug.Trace
import Prelude
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State.Strict as S --Xhiding(ap)
--Ximport Control.Monad as S hiding(ap)
--Ximport Compat
--Ximport GHC.Stack
--Ximport Debug.Trace

import MicroHs.Parse
import MicroHs.Exp
import MicroHs.TypeCheck

type LDef = (Ident, Exp)

desugar :: TModule [EDef] -> TModule [LDef]
desugar atm =
  case atm of
    TModule mn imps exps ds -> TModule mn imps exps (concatMap (dsDef mn) ds)

dsDef :: IdentModule -> EDef -> [LDef]
dsDef mn adef =
  case adef of
    Data _ cs ->
      let
        f i = "$f" ++ showInt i
        fs = [f i | (i, _) <- zip (enumFrom 0) cs]
        dsConstr i cts =
          case cts of
            (c, ts) ->
              let
                xs = ["$x" ++ showInt j | (j, _) <- zip (enumFrom 0) ts]
              in (qual mn c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      in  zipWith dsConstr (enumFrom 0) cs
    Type _ _ -> []
    Fcn fxs e ->
      case fxs of
        (f, xs) -> [(f, lams xs $ dsExpr e)]
    Sign _ _ -> []
    Import _ -> []

dsBind :: EBind -> [LDef]
dsBind abind =
  case abind of
    BFcn (f, xs) e -> [(f, lams xs $ dsExpr e)]
    BPat p e ->
      let
        v = newVar (allVarsBind abind)
        de = (v, dsExpr e)
        ds = [ (i, dsExpr (ECase (EVar v) [(p, EVar i)])) | i <- patVars p ]
      in  de : ds

dsExpr :: Expr -> Exp
dsExpr aexpr =
  case aexpr of
    EVar i -> Var i
    EApp f a -> App (dsExpr f) (dsExpr a)
    ELam x e -> Lam x (dsExpr e)
    EInt i -> Int i
    EChar c -> Int (ord c)
    ECase e as -> dsCase e as
-- For now, just sequential bindings; each recursive
    ELet ads e ->
      case ads of
        [] -> dsExpr e
        d:ds ->
          let
            dsd = dsBind d
            de = dsExpr (ELet ds e)
            def ir a =
                case ir of
                  (i, r) -> App (Lam i a) (App (Prim "Y") (Lam i r))
          in  foldr def de dsd
    EList es ->
      foldr (app2 cCons) cNil $ map dsExpr es
    ETuple es -> Lam "_f" $ foldl App (Var "_f") $ map dsExpr es
    EStr cs -> dsExpr $ EList $ map EChar cs
    EDo mn astmts ->
      case astmts of
        [] -> error "empty do"
        stmt : stmts ->
          case stmt of
            SBind p e ->
              if null stmts then error "do without final expression"
              else
                let
                  nv = newVar (allVarsExpr aexpr)
                  body = ECase (EVar nv) [(p, EDo mn stmts), (EVar dummyIdent, eError "dopat")]
                in  dsExpr $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam nv body)
            SThen e ->
              if null stmts then
                dsExpr e
              else
                dsExpr $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn stmts)
            SLet ds ->
              if null stmts then error "do without final expression" else
                dsExpr $ ELet ds (EDo mn stmts)

    EPrim s -> Prim s
    ESectL e op ->
      App (dsExpr (EVar op)) (dsExpr e)
    ESectR op e ->
      app2 cFlip (dsExpr (EVar op)) (dsExpr e)
    EIf e1 e2 e3 ->
      app2 (dsExpr e1) (dsExpr e3) (dsExpr e2)
    ECompr e astmts ->
      case astmts of
        [] -> dsExpr (EList [e])
        stmt : stmts ->
          case stmt of
            SBind p b ->
              let
                nv = newVar (allVarsExpr aexpr)
                body = ECase (EVar nv) [(p, ECompr e stmts), (EVar dummyIdent, EList [])]
              in app2 (Var "Data.List.concatMap") (dsExpr (ELam nv body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (ECompr e stmts) (EList []))
            SLet ds ->
              dsExpr (ELet ds (ECompr e stmts))
    EBad msg -> error $ "complex case not implemented: " ++ msg
    EAt _ _ -> undefined
    EUVar _ -> undefined
    ECon c ->
      if eqChar (head (conIdent c)) ',' then
        undefined  -- not implemented yet
      else
        Var (conIdent c)

spatVars :: SPat -> [Ident]
spatVars ap =
  case ap of
    SPat _ is -> is

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

-- Handle special syntax for lists and tuples
dsPat :: EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EList ps -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (length ps)) ps
    _ -> impossible

consCon :: EPat
consCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon $ Con [(n, 0), (c, 2)] c

nilCon :: EPat
nilCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon $ Con [(n, 0), (c, 2)] n

tupleCon :: Int -> EPat
tupleCon n =
  let
    c = tupleConstr n
  in ECon $ Con [(c, n)] c

dummyIdent :: Ident
dummyIdent = "_"

eError :: String -> Expr
eError s = EApp (EPrim "error") (EStr s)

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: [Ident] -> [Ident]
newVars is = deleteFirstsBy eqIdent [ "q" ++ showInt i | i <- enumFrom 1 ] is

newVar :: [Ident] -> Ident
newVar = head . newVars

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> i ++ " = " ++ showExp e

----------------

dsCase :: Expr -> [ECaseArm] -> Exp
dsCase ae as =
  let
    r = runS (allVarsExpr (ECase ae as)) [dsExpr ae] [([dsPat p], dsExpr e) | (p, e) <- as]
  in --trace (showExp r) $
     r
     

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Exp)
type Matrix = [Arm]

newIdents :: Int -> M [Ident]
newIdents n = S.do
  is <- get
  put (drop n is)
  S.return (take n is)

newIdent :: M Ident
newIdent = S.do
  is <- get
  put (tail is)
  S.return (head is)

runS :: [Ident] -> [Exp] -> Matrix -> Exp
runS used ss mtrx =
  let
    supply = deleteFirstsBy eqIdent [ "x" ++ showInt i | i <- enumFrom 1 ] used
--    ds :: [Exp] -> [Exp] -> M Exp
    ds xs aes =
      case aes of
        []   -> --letBind (S.return eMatchErr) $ \ d ->
                dsMatrix eMatchErr (reverse xs) mtrx
        e:es -> letBind (S.return e) $ \ x -> ds (x:xs) es
  in S.evalState (ds [] ss) supply

data SPat = SPat Con [Ident]    -- simple pattern
  --Xderiving(Show, Eq)

-- Desugar a pattern matrix.
-- The input is a (usually identifier) vector e1, ..., en
-- and patterns matrix p11, ..., p1n   -> e1
--                     p21, ..., p2n
--                     pm1, ..., pmn   -> em
-- Note that the RHSs are of type Exp.
dsMatrix :: Exp -> [Exp] -> Matrix -> M Exp
dsMatrix dflt iis aarms =
 if null aarms then
   S.return dflt
 else
 case iis of
 [] -> S.return (snd (head aarms))
 i:is -> S.do
  let
    -- XXX handle EAt
    (arms, darms, rarms) = splitArms aarms
--    ndarms = map (\ xxx -> let { (EVar x : ps, ed) = xxx } in (ps, substAlpha x i ed)) darms
    ndarms = map (\ xxx -> case xxx of { (pps, ed) -> case pps of {p : ps -> case p of { EVar x -> (ps, substAlpha x i ed) }}} ) darms
--  traceM ("split " ++ show (arms, darms, rarms))
  letBind (dsMatrix dflt iis rarms) $ \ drest ->
    letBind (dsMatrix drest is ndarms) $ \ ndflt ->
     if null arms then
       S.return ndflt
     else S.do
      let
        grps = groupEq (on eqIdent (conIdent . pConOf . head . fst)) arms
        oneGroup grp = S.do
          let
            (pat:_, _) : _ = grp
            con = pConOf pat
          xs <- newIdents (conArity con)
          let
            one arg =
              case arg of
                (p : ps, e) ->
                  case p of
                    EAt a pp -> one (pp:ps, substAlpha a i e)
                    _        -> (pArgs p ++ ps, e)
                _ -> impossible
          cexp <- dsMatrix ndflt (map Var xs ++ is) (map one grp)
          S.return (SPat con xs, cexp)
--      traceM $ "grps " ++ show grps
      narms <- S.mapM oneGroup grps
      S.return $ mkCase i narms ndflt

eMatchErr :: Exp
eMatchErr = dsExpr $ EApp (EPrim "error") (EStr "no match")

-- If the first expression isn't a variable, the use
-- a let binding and pass variable to f.
letBind :: M Exp -> (Exp -> M Exp) -> M Exp
letBind me f = S.do
  e <- me
  if cheap e then
    f e
   else S.do
    x <- newIdent
    r <- f (Var x)
    S.return $ eLet x e r

cheap :: Exp -> Bool
cheap ae =
  case ae of
    Var _ -> True
    Int _ -> True
    Prim _ -> True
    App f _ ->
      case f of
        Prim _ -> True
        _ -> False
    Lam _ _ -> False

mkCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
mkCase var pes dflt =
--  trace ("mkCase " ++ show pes) $
  let
      (SPat (Con cs _) _, _) : _ = pes
      arm ck =
        let
          (c, k) = ck
          (vs, rhs) = head $ [ (xs, e) | (SPat (Con _ i) xs, e) <- pes, eqIdent c i ] ++
                             [ (replicate k dummyIdent, dflt) ]
        in if length vs /= k then error "bad arity" else
           (SPat (Con cs c) vs, rhs)
  in  eCase var (map arm cs)

eCase :: Exp -> [(SPat, Exp)] -> Exp
eCase e as = apps e [lams xs r | (SPat _ xs, r) <- as ]

-- Split the matrix into segments so each first column has initially patterns
-- followed by a single default case.
splitArms :: Matrix -> (Matrix, Matrix, Matrix)
splitArms am =
  let
    ps  = takeWhile (not . isPVar . head . fst) am
    nps = dropWhile (not . isPVar . head . fst) am
    ds  = takeWhile (      isPVar . head . fst) nps
    rs  = dropWhile (      isPVar . head . fst) nps
  in (ps, ds, rs)

-- Change from x to y inside e.
-- XXX Doing it at runtime.
substAlpha :: Ident -> Exp -> Exp -> Exp
substAlpha x y e =
  if eqIdent x dummyIdent then
    e
  else
    substExp x y e

eLet :: Ident -> Exp -> Exp -> Exp
eLet i e b =
  if eqIdent i dummyIdent then
    b
  else
    let
      r = App (Lam i b) e
    in case b of
         Var j -> if eqIdent i j then e else r
         _ -> r

pConOf :: EPat -> Con
pConOf ap =
  case ap of
    ECon c -> c
    EAt _ p -> pConOf p
    EApp p _ -> pConOf p
    _ -> impossible

pArgs :: EPat -> [EPat]
pArgs ap =
  case ap of
    ECon _ -> []
    EApp f a -> pArgs f ++ [a]
    _ -> impossible

-- XXX quadratic
groupEq :: forall a . (a -> a -> Bool) -> [a] -> [[a]]
groupEq eq axs =
  case axs of
    [] -> []
    x:xs ->
      let
        (es, ns) = partition (eq x) xs
      in (x:es) : groupEq eq ns
