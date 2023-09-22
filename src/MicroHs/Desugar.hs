-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-imports -Wno-dodgy-imports #-}
module MicroHs.Desugar(
  desugar,
  LDef, showLDefs,
  ) where
import Prelude --Xhiding(showList)
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
import MicroHs.Graph
import MicroHs.Ident
import MicroHs.TypeCheck

type LDef = (Ident, Exp)

desugar :: TModule [EDef] -> TModule [LDef]
desugar atm =
  case atm of
    TModule mn fxs tys syns vals ds ->
      TModule mn fxs tys syns vals $ checkDup $ concatMap (dsDef mn) ds

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
    Fcn f eqns -> [(f, dsEqns (getSLocIdent f) eqns)]
    Sign _ _ -> []
    Import _ -> []
    ForImp ie i _ -> [(i, Lit $ LForImp ie)]
    Infix _ _ -> []

oneAlt :: Expr -> EAlts
oneAlt e = EAlts [([], e)] []

dsBind :: Ident -> EBind -> [LDef]
dsBind v abind =
  case abind of
    BFcn f eqns -> [(f, dsEqns (getSLocIdent f) eqns)]
    BPat p e ->
      let
        de = (v, dsExpr e)
        ds = [ (i, dsExpr (ECase (EVar v) [(p, oneAlt $ EVar i)])) | i <- patVars p ]
      in  de : ds
    BSign _ _ -> []

dsEqns :: SLoc -> [Eqn] -> Exp
dsEqns loc eqns =
  case eqns of
    Eqn aps _ : _ ->
      let
        vs = allVarsBind $ BFcn (mkIdent "") eqns
        xs = take (length aps) $ newVars "q" vs
        mkArm (Eqn ps alts) =
          let ps' = map dsPat ps
          in  (ps', dsAlts alts, hasGuards alts || any hasLit ps')
        ex = runS loc (vs ++ xs) (map Var xs) (map mkArm eqns)
      in foldr Lam ex xs
    _ -> impossible

hasGuards :: EAlts -> Bool
hasGuards (EAlts [([], _)] _) = False
hasGuards _ = True

hasLit :: EPat -> Bool
hasLit (ELit _ _) = True
hasLit (EVar _) = False
hasLit (ECon _) = False
hasLit (EApp f a) = hasLit f || hasLit a
hasLit (EAt _ p) = hasLit p
hasLit _ = impossible

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
dsBinds [] ret = ret
dsBinds ads ret =
  let
    avs = concatMap allVarsBind ads
    pvs = newVars "p" avs
    mvs = newVars "m" avs
    ds = concat $ zipWith dsBind pvs ads
    node ie@(i, e) = (ie, i, freeVars e)
    gr = map node $ checkDup ds
    asccs = stronglyConnComp leIdent gr
    loop _ [] = ret
    loop vs (AcyclicSCC (i, e) : sccs) =
      letE i e $ loop vs sccs
    loop vs (CyclicSCC [(i, e)] : sccs) =
      letRecE i e $ loop vs sccs
    loop vvs (CyclicSCC ies : sccs) =
      let (v:vs) = vvs
      in mutualRec v ies (loop vs sccs)
  in loop mvs asccs

letE :: Ident -> Exp -> Exp -> Exp
letE i e b = App (Lam i b) e

letRecE :: Ident -> Exp -> Exp -> Exp
letRecE i e b = letE i (App (Lit (LPrim "Y")) (Lam i e)) b

-- Do mutual recursion by tupling up all the definitions.
--  let f = ... g ...
--      g = ... f ...
--  in  body
-- turns into
--  letrec v =
--        let f = sel_0_2 v
--            g = sel_1_2 v
--        in  (... g ..., ... f ...)
--  in
--    let f = sel_0_2 v
--        g = sel_1_2 v
--    in  body
mutualRec :: Ident -> [LDef] -> Exp -> Exp
mutualRec v ies body =
  let (is, es) = unzip ies
      n = length is
      ev = Var v
      one m i = letE i (mkTupleSel m n ev)
      bnds = foldr (.) id $ zipWith one [0..] is
  in  letRecE v (bnds $ mkTuple es) $
      bnds body

dsExpr :: Expr -> Exp
dsExpr aexpr =
  case aexpr of
    EVar i -> Var i
    EApp f a -> App (dsExpr f) (dsExpr a)
    ELam xs e -> dsLam (getSLocExpr aexpr) xs e
    ELit _ (LChar c) -> Lit (LInt (ord c))
    ELit _ l -> Lit l
    ECase e as -> dsCase (getSLocExpr aexpr) e as
    ELet ads e -> dsBinds ads (dsExpr e)
    ETuple es -> Lam (mkIdent "$f") $ foldl App (Var $ mkIdent "$f") $ map dsExpr es
    EIf e1 e2 e3 ->
      app2 (dsExpr e1) (dsExpr e3) (dsExpr e2)
    EListish (LList es) -> foldr (app2 cCons) cNil $ map dsExpr es
    EListish (LCompr e astmts) ->
      case astmts of
        [] -> dsExpr (EListish (LList [e]))
        stmt : stmts ->
          case stmt of
            SBind p b ->
              let
                nv = newVar (allVarsExpr aexpr)
                body = ECase (EVar nv) [(p, oneAlt $ EListish (LCompr e stmts)), (EVar dummyIdent, oneAlt $ EListish (LList []))]
              in app2 (Var (mkIdent "Data.List.concatMap")) (dsExpr (ELam [EVar nv] body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (EListish (LCompr e stmts)) (EListish (LList [])))
            SLet ds ->
              dsExpr (ELet ds (EListish (LCompr e stmts)))
    ECon c ->
      let
        ci = conIdent c
      in
        if eqChar (head $ unIdent ci) ',' then
          let
            xs = [mkIdent ("x" ++ showInt i) | i <- enumFromTo 1 (untupleConstr ci) ]
            body = mkTuple $ map Var xs
          in foldr Lam body xs
        else
          Var (conIdent c)
    _ -> impossible

-- Use tuple encoding to make a tuple
mkTuple :: [Exp] -> Exp
mkTuple = Lam (mkIdent "$f") . foldl App (Var (mkIdent "$f"))

-- Select component m from an n-tuple
mkTupleSel :: Int -> Int -> Exp -> Exp
mkTupleSel m n tup =
  let
    xs = [mkIdent ("x" ++ showInt i) | i <- enumFromTo 1 n ]
  in App tup (foldr Lam (Var (xs !! m)) xs)

dsLam :: SLoc -> [EPat] -> Expr -> Exp
dsLam loc ps e =
  let
    vs = allVarsExpr (ELam ps e)
    xs = take (length ps) (newVars "l" vs)
    ps' = map dsPat ps
    ex = runS loc (vs ++ xs) (map Var xs) [(ps', dsAlts $ oneAlt e, any hasLit ps')]
  in foldr Lam ex xs

-- Handle special syntax for lists and tuples
dsPat :: --XHasCallStack =>
         EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EListish (LList ps) -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (getSLocExpr ap) (length ps)) ps
    EAt i p -> EAt i (dsPat p)
    ELit loc (LStr cs) | length cs < 2 -> dsPat (EListish (LList (map (ELit loc . LChar) cs)))
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

tupleCon :: SLoc -> Int -> EPat
tupleCon loc n =
  let
    c = tupleConstr loc n
  in ECon $ ConData [(c, n)] c

dummyIdent :: Ident
dummyIdent = mkIdent "_"

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: String -> [Ident] -> [Ident]
newVars s is = deleteAllsBy eqIdent [ mkIdent (s ++ showInt i) | i <- enumFrom 1 ] is

newVar :: [Ident] -> Ident
newVar = head . newVars "q"

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> showIdent i ++ " = " ++ showExp e

----------------

dsCase :: SLoc -> Expr -> [ECaseArm] -> Exp
dsCase loc ae as =
  runS loc (allVarsExpr (ECase ae as)) [dsExpr ae] (map mkArm as)
  where
    mkArm (p, alts) =
      let p' = dsPat p
      in  ([p'], dsAlts alts, hasGuards alts || hasLit p')

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Exp -> Exp, Bool)  -- boolean indicates that the arm has guards
type Matrix = [Arm]

--showArm :: Arm -> String
--showArm (ps, _, b) = showList showExpr ps ++ "," ++ showBool b

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

runS :: SLoc -> [Ident] -> [Exp] -> Matrix -> Exp
runS loc used ss mtrx =
  let
    supply = newVars "x" used
    ds xs aes =
      case aes of
        []   -> dsMatrix (eMatchErr loc) (reverse xs) mtrx
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

eMatchErr :: SLoc -> Exp
eMatchErr (SLoc fn l c) =
  App (App (App (Var (mkIdent "Prelude._noMatch")) (Lit (LStr fn))) (Lit (LInt l))) (Lit (LInt c))

-- If the first expression isn't a variable/literal, then use
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
    _ -> False

-- Could use Prim "==", but that misses out some optimizations
eEqInt :: Exp
eEqInt = Var $ mkIdent "Data.Int.=="

eEqChar :: Exp
eEqChar = Var $ mkIdent "Data.Char.eqChar"

eEqStr :: Exp
eEqStr = --Var $ mkIdent "Text.String.eqString"
         Lit (LPrim "equal")

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
          [_] -> substExp i e b   -- single occurrence, substitute  XXX could be worse if under lambda
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
      case partition (eq x) xs of
        (es, ns) -> (x:es) : groupEq eq ns

getDups :: forall a . (a -> a -> Bool) -> [a] -> [[a]]
getDups eq = filter ((> 1) . length) . groupEq eq

checkDup :: [LDef] -> [LDef]
checkDup ds =
  case getDups eqIdent (filter (not . eqIdent dummyIdent) $ map fst ds) of
    [] -> ds
    (i1:i2:_) : _ ->
      errorMessage (getSLocIdent i1) $ "Duplicate " ++ showIdent i1 ++ " " ++ showSLoc (getSLocIdent i2)
    _ -> error "checkDup"
