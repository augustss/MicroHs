-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-imports -Wno-dodgy-imports #-}
module MicroHs.Desugar(
  desugar,
  LDef, showLDefs
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

import MicroHs.Expr
import MicroHs.Exp
import MicroHs.Ident
import MicroHs.TypeCheck

type LDef = (Ident, Exp)

desugar :: TModule [EDef] -> TModule [LDef]
desugar atm =
  case atm of
    TModule mn tys syns vals ds -> TModule mn tys syns vals (concatMap (dsDef mn) ds)

dsDef :: IdentModule -> EDef -> [LDef]
dsDef mn adef =
  case adef of
    Data _ cs ->
      let
        f i = mkIdent ("$f" ++ showInt i)
        fs = [f i | (i, _) <- zip (enumFrom 0) cs]
        dsConstr i (c, ts) =
          let
            xs = [mkIdent ("$x" ++ showInt j) | (j, _) <- zip (enumFrom 0) ts]
          in (qualIdent mn c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      in  zipWith dsConstr (enumFrom 0) cs
    Newtype _ c _ -> [ (qualIdent mn c, Lit (LPrim "I")) ]
    Type _ _ -> []
    Fcn f eqns -> [(f, dsEqns eqns)]
    Sign _ _ -> []
    Import _ -> []

oneAlt :: Expr -> EAlts
oneAlt e = EAlts [([], e)] []

dsBind :: EBind -> [LDef]
dsBind abind =
  case abind of
    BFcn f eqns -> [(f, dsEqns eqns)]
    BPat p e ->
      let
        v = newVar (allVarsBind abind)
        de = (v, dsExpr e)
        ds = [ (i, dsExpr (ECase (EVar v) [(p, oneAlt $ EVar i)])) | i <- patVars p ]
      in  de : ds

dsEqns :: [Eqn] -> Exp
dsEqns eqns =
  case eqns of
    Eqn aps _ : _ ->
      let
        vs = allVarsBind $ BFcn (mkIdent "") eqns
        xs = take (length aps) $ newVars vs
        ex = runS (vs ++ xs) (map Var xs) [(map dsPat ps, dsAlts alts, hasGuards alts) | Eqn ps alts <- eqns]
      in foldr Lam ex xs
    _ -> impossible

hasGuards :: EAlts -> Bool
hasGuards (EAlts [([], _)] _) = False
hasGuards _ = True

dsAlts :: EAlts -> (Exp -> Exp)
dsAlts (EAlts alts bs) = dsBinds bs . dsAltsL alts

dsAltsL :: [EAlt] -> (Exp -> Exp)
dsAltsL []                 dflt = dflt
dsAltsL [([], e)]             _ = dsExpr e  -- fast special case
dsAltsL ((ss, rhs) : alts) dflt =
  let
    erest = dsAltsL alts dflt
    x = newVar (allVarsExp erest)
  in eLet x erest (dsExpr $ dsAlt (EVar x) ss rhs)

dsAlt :: Expr -> [EStmt] -> Expr -> Expr
dsAlt _ [] rhs = rhs
dsAlt dflt (SBind p e : ss) rhs = ECase e [(p, EAlts [(ss, rhs)] []), (EVar dummyIdent, oneAlt dflt)]
dsAlt dflt (SThen (EVar i) : ss) rhs | eqIdent i (mkIdent "Data.Bool.otherwise") = dsAlt dflt ss rhs
dsAlt dflt (SThen e   : ss) rhs = EIf e (dsAlt dflt ss rhs) dflt
dsAlt dflt (SLet bs   : ss) rhs = ELet bs (dsAlt dflt ss rhs)

dsBinds :: [EBind] -> Exp -> Exp
dsBinds ads ret =
  case ads of
    [] -> ret
    d:ds ->
      let
        dsd = dsBind d
        de = dsBinds ds ret
        def ir a =
          case ir of
            (i, r) -> App (Lam i a) (App (Lit (LPrim "Y")) (Lam i r))
      in  foldr def de dsd

dsExpr :: Expr -> Exp
dsExpr aexpr =
  case aexpr of
    EVar i -> Var i
    EApp f a -> App (dsExpr f) (dsExpr a)
    ELam xs e -> dsLam xs e
    ELit _ (LChar c) -> Lit (LInt (ord c))
--    ELit _ (LStr cs) -> dsExpr $ EList $ map (ELit . LChar) cs
    ELit _ l -> Lit l
    ECase e as -> dsCase e as
-- For now, just sequential bindings; each recursive
    ELet ads e -> dsBinds ads (dsExpr e)
    EList es -> foldr (app2 cCons) cNil $ map dsExpr es
    ETuple es -> Lam (mkIdent "$f") $ foldl App (Var $ mkIdent "$f") $ map dsExpr es
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
                body = ECase (EVar nv) [(p, oneAlt $ ECompr e stmts), (EVar dummyIdent, oneAlt $ EList [])]
              in app2 (Var (mkIdent "Data.List.concatMap")) (dsExpr (ELam [EVar nv] body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (ECompr e stmts) (EList []))
            SLet ds ->
              dsExpr (ELet ds (ECompr e stmts))
    ESign e _ -> dsExpr e
    ECon c ->
      let
        ci = conIdent c
      in
        if eqChar (head $ unIdent ci) ',' then
          let
            xs = [mkIdent ("x" ++ showInt i) | i <- enumFromTo 1 (untupleConstr ci) ]
            body = Lam (mkIdent "$f") $ foldl App (Var (mkIdent "$f")) $ map Var xs
          in foldr Lam body xs
        else
          Var (conIdent c)
    _ -> impossible

dsLam :: [EPat] -> Expr -> Exp
dsLam ps e =
  let
    vs = allVarsExpr (ELam ps e)
    xs = take (length ps) (newVars vs)
    ex = runS (vs ++ xs) (map Var xs) [(map dsPat ps, dsAlts $ oneAlt e, False)]
  in foldr Lam ex xs

-- Handle special syntax for lists and tuples
dsPat :: --XHasCallStack =>
         EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EList ps -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (length ps)) ps
    EAt i p -> EAt i (dsPat p)
    ELit _ _ -> ap
    _ -> impossible

consCon :: EPat
consCon =
  let
    n = mkIdent "Data.List.[]"
    c = mkIdent "Data.List.:"
  in ECon $ ConData [(n, 0), (c, 2)] c

nilCon :: EPat
nilCon =
  let
    n = mkIdent "Data.List.[]"
    c = mkIdent "Data.List.:"
  in ECon $ ConData [(n, 0), (c, 2)] n

tupleCon :: Int -> EPat
tupleCon n =
  let
    c = tupleConstr n
  in ECon $ ConData [(c, n)] c

dummyIdent :: Ident
dummyIdent = mkIdent "_"

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: [Ident] -> [Ident]
newVars is = deleteFirstsBy eqIdent [ mkIdent ("q" ++ showInt i) | i <- enumFrom 1 ] is

newVar :: [Ident] -> Ident
newVar = head . newVars

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> showIdent i ++ " = " ++ showExp e

----------------

dsCase :: Expr -> [ECaseArm] -> Exp
dsCase ae as =
  let
    r = runS (allVarsExpr (ECase ae as)) [dsExpr ae] [([dsPat p], dsAlts alts, hasGuards alts) | (p, alts) <- as]
  in --trace (showExp r) $
     r

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Exp -> Exp, Bool)  -- boolean indicates that the arm has guards
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
  --trace ("runS " ++ show (ss, mtrx)) $
  let
    supply = deleteFirstsBy eqIdent [ mkIdent ("x" ++ showInt i) | i <- enumFrom 1 ] used
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
dsMatrix :: --XHasCallStack =>
            Exp -> [Exp] -> Matrix -> M Exp
dsMatrix dflt iis aarms =
 if null aarms then
   S.return dflt
 else
 case iis of
 [] -> let { (_, f, _) : _ = aarms } in S.return $ f dflt
 i:is -> S.do
  let
    (arms, darms, rarms) = splitArms aarms
    ndarms = map (\ (EVar x : ps, ed, g) -> (ps, substAlpha x i . ed, g) ) darms
--  traceM ("split " ++ show (arms, darms, rarms))
  letBind (dsMatrix dflt iis rarms) $ \ drest ->
    letBind (dsMatrix drest is ndarms) $ \ ndflt ->
     if null arms then
       S.return ndflt
     else S.do
      let
        idOf (p:_, _, _) = pConOf p
        idOf _ = impossible
        grps = groupEq (on eqCon idOf) arms
        oneGroup grp = S.do
          let
            (pat:_, _, _) : _ = grp
            con = pConOf pat
          xs <- newIdents (conArity con)
          let
            one arg =
              case arg of
                (p : ps, e, g) ->
                  case p of
                    EAt a pp -> one (pp:ps, substAlpha a i . e, g)
                    _        -> (pArgs p ++ ps, e, g)
                _ -> impossible
          cexp <- dsMatrix ndflt (map Var xs ++ is) (map one grp)
          S.return (SPat con xs, cexp)
--      traceM $ "grps " ++ show grps
      narms <- S.mapM oneGroup grps
      S.return $ mkCase i narms ndflt

eMatchErr :: Exp
eMatchErr = App (Lit (LPrim "error")) (Lit (LStr "no match"))

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
    Lit _ -> True
--    App (Lit _) _ -> True
    _ -> False

-- Could use Prim "==", but that misses out some optimizations
eEqInt :: Exp
eEqInt = Var $ mkIdent "Data.Int.=="

eEqChar :: Exp
eEqChar = Var $ mkIdent "Data.Char.eqChar"

eEqStr :: Exp
eEqStr = Var $ mkIdent "Text.String.eqString"

mkCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
mkCase var pes dflt =
  --trace ("mkCase " ++ show pes) $
  case pes of
    [] -> dflt
    [(SPat (ConNew _) [x], arhs)] -> eLet x var arhs
    (SPat (ConLit l) _,   arhs) : rpes -> 
      let
        cond =
          case l of
            LInt  _ -> app2 eEqInt  var (Lit l)
            LChar c -> app2 eEqChar var (Lit (LInt (ord c)))
            LStr  _ -> app2 eEqStr  var (Lit l)
            _ -> impossible
      in app2 cond (mkCase var rpes dflt) arhs
    (SPat (ConData cs _) _, _) : _ ->
      let
        arm ck =
          let
            (c, k) = ck
            (vs, rhs) = head $ [ (xs, e) | (SPat (ConData _ i) xs, e) <- pes, eqIdent c i ] ++
                               [ (replicate k dummyIdent, dflt) ]
          in (SPat (ConData cs c) vs, rhs)
      in  eCase var (map arm cs)
    _ -> impossible

eCase :: Exp -> [(SPat, Exp)] -> Exp
eCase e as =
  --trace ("eCase " ++ showExp e ++ "\n" ++
  --       unlines [ unwords (conIdent c : xs) ++ " -> " ++ showExp r | (SPat c xs, r) <- as ]) $
  apps e [lams xs r | (SPat _ xs, r) <- as ]

-- Split the matrix into segments so each first column has initially patterns -- followed by variables, followed by the rest.
splitArms :: Matrix -> (Matrix, Matrix, Matrix)
splitArms am =
  let
    isConPat (p:_, _, _) = not (isPVar p)
    isConPat _ = impossible
    (ps, nps) = span isConPat am
    loop xs [] = (reverse xs, [])
    loop xs pps@(pg@(p:_, _, g) : rps) | not (isPVar p) = (reverse xs, pps)
                                       | otherwise = if g then (reverse (pg:xs), rps)
                                                         else loop (pg:xs) rps
    loop _ _ = impossible
    (ds, rs)  = loop [] nps
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
    case b of
      Var j | eqIdent i j -> e
      _ ->
        case filter (eqIdent i) (freeVars b) of
          []  -> b                -- no occurences, no need to bind
          [_] -> substExp i e b   -- single occurrence, substitute  XXX coule be worse if under lambda
          _   -> App (Lam i b) e  -- just use a beta redex

pConOf :: --XHasCallStack =>
          EPat -> Con
pConOf ap =
  case ap of
    ECon c -> c
    EAt _ p -> pConOf p
    EApp p _ -> pConOf p
    ELit _ l -> ConLit l
    _ -> impossible

pArgs :: EPat -> [EPat]
pArgs ap =
  case ap of
    ECon _ -> []
    EApp f a -> pArgs f ++ [a]
    ELit _ _ -> []
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
