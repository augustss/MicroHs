-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports -Wno-dodgy-imports #-}
module MicroHs.Desugar(
  desugar,
  LDef, showLDefs,
  encodeInteger,
  ) where
import Prelude
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Monad.State.Strict as S --Xhiding(ap)
--Ximport Control.Monad as S hiding(ap)
--Ximport Compat
--Ximport GHC.Stack
import Debug.Trace

import MicroHs.Expr
import MicroHs.Exp
import MicroHs.Graph
import MicroHs.Ident
import MicroHs.TypeCheck

type LDef = (Ident, Exp)

desugar :: TModule [EDef] -> TModule [LDef]
desugar atm =
  case atm of
    TModule mn fxs tys syns clss insts vals ds ->
      TModule mn fxs tys syns clss insts vals $ map lazier $ checkDup $ concatMap (dsDef mn) ds

dsDef :: IdentModule -> EDef -> [LDef]
dsDef mn adef =
  case adef of
    Data _ cs ->
      let
        f i = mkIdent ("$f" ++ show i)
        fs = [f i | (i, _) <- zip [0::Int ..] cs]
        dsConstr i (Constr _ ctx c ets) =
          let
            ss = (if null ctx then [] else [False]) ++
                 map fst (either id (map snd) ets)   -- strict flags
            xs = [mkIdent ("$x" ++ show j) | (j, _) <- zip [0::Int ..] ss]
            strict (False:ys) (_:is) e = strict ys is e
            strict (True:ys)  (x:is) e = App (App (Lit (LPrim "seq")) (Var x)) (strict ys is e)
            strict _ _ e = e
          in (qualIdent mn c, lams xs $ strict ss xs $ lams fs $ apps (Var (f i)) (map Var xs))
      in  zipWith dsConstr [0::Int ..] cs
    Newtype _ (Constr _ _ c _) -> [ (qualIdent mn c, Lit (LPrim "I")) ]
    Type _ _ -> []
    Fcn f eqns -> [(f, dsEqns (getSLoc f) eqns)]
    Sign _ _ -> []
    Import _ -> []
    ForImp ie i _ -> [(i, Lit $ LForImp ie)]
    Infix _ _ -> []
    Class ctx (c, _) _ bs ->
      let f = mkIdent "$f"
          meths :: [Ident]
          meths = [ qualIdent mn i | (BSign i _) <- bs ]
          supers :: [Ident]
          supers = [ qualIdent mn $ mkSuperSel c i | i <- [1 .. length ctx] ]
          xs = [ mkIdent ("$x" ++ show j) | j <- [ 1 .. length ctx + length meths ] ]
      in  (qualIdent mn $ mkClassConstructor c, lams xs $ Lam f $ apps (Var f) (map Var xs)) :
          zipWith (\ i x -> (expectQualified i, Lam f $ App (Var f) (lams xs $ Var x))) (supers ++ meths) xs
    Instance _ _ _ _ -> []
    Default _ -> []

oneAlt :: Expr -> EAlts
oneAlt e = EAlts [([], e)] []

dsBind :: Ident -> EBind -> [LDef]
dsBind v abind =
  case abind of
    BFcn f eqns -> [(f, dsEqns (getSLoc f) eqns)]
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
        xs = take (length aps) $ newVars "$q" vs
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
dsAlt dflt (SThen (EVar i) : ss) rhs | isIdent "Data.Bool.otherwise" i = dsAlt dflt ss rhs
dsAlt dflt (SThen e   : ss) rhs = EIf e (dsAlt dflt ss rhs) dflt
dsAlt dflt (SLet bs   : ss) rhs = ELet bs (dsAlt dflt ss rhs)

dsBinds :: [EBind] -> Exp -> Exp
dsBinds [] ret = ret
dsBinds ads ret =
  let
    avs = concatMap allVarsBind ads
    pvs = newVars "$p" avs
    mvs = newVars "$m" avs
    ds = concat $ zipWith dsBind pvs ads
    node ie@(i, e) = (ie, i, freeVars e)
    gr = map node $ checkDup ds
    asccs = stronglyConnComp (<=) gr
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
letE i e b = eLet i e b          -- do some minor optimizations
             --App (Lam i b) e

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
      one m i = letE i (mkTupleSelE m n ev)
      bnds = foldr (.) id $ zipWith one [0..] is
  in  letRecE v (bnds $ mkTupleE es) $
      bnds body

encodeInteger :: Integer -> Exp
encodeInteger i | toInteger (minBound::Int) <= i && i < toInteger (maxBound::Int) =
--  trace ("*** small integer " ++ show i) $
  App (Var (mkIdent "Data.Integer_Type._intToInteger")) (Lit (LInt (_integerToInt i)))
                | otherwise =
--  trace ("*** large integer " ++ show i) $
  App (Var (mkIdent "Data.Integer._intListToInteger")) (encodeList (map (Lit . LInt) (_integerToIntList i)))

encodeRational :: Rational -> Exp
encodeRational r =
  App (App (Var (mkIdent "Data.Ratio_Type._mkRational")) (encodeInteger (numerator r))) (encodeInteger (denominator r))

dsExpr :: Expr -> Exp
dsExpr aexpr =
  case aexpr of
    EVar i -> Var i
    EApp f a -> App (dsExpr f) (dsExpr a)
    ELam qs -> dsEqns (getSLoc aexpr) qs
    ELit _ (LChar c) -> Lit (LInt (ord c))
    ELit _ (LInteger i) -> encodeInteger i
    ELit _ (LRat i) -> encodeRational i
    ELit _ l -> Lit l
    ECase e as -> dsCase (getSLoc aexpr) e as
    ELet ads e -> dsBinds ads (dsExpr e)
    ETuple es -> Lam (mkIdent "$f") $ foldl App (Var $ mkIdent "$f") $ map dsExpr es
    EIf e1 e2 e3 ->
      app2 (dsExpr e1) (dsExpr e3) (dsExpr e2)
    EListish (LList es) -> encodeList $ map dsExpr es
    EListish (LCompr e astmts) ->
      case astmts of
        [] -> dsExpr (EListish (LList [e]))
        stmt : stmts ->
          case stmt of
            SBind p b ->
              let
                nv = newVar (allVarsExpr aexpr)
                body = ECase (EVar nv) [(p, oneAlt $ EListish (LCompr e stmts)), (EVar dummyIdent, oneAlt $ EListish (LList []))]
              in app2 (Var (mkIdent "Data.List.concatMap")) (dsExpr (eLam [EVar nv] body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (EListish (LCompr e stmts)) (EListish (LList [])))
            SLet ds ->
              dsExpr (ELet ds (EListish (LCompr e stmts)))
    ECon c ->
      let
        ci = conIdent c
      in
        case getTupleConstr ci of
          Just n ->
            let
              xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
              body = mkTupleE $ map Var xs
            in foldr Lam body xs
          Nothing -> Var (conIdent c)
    _ -> impossible

-- Use tuple encoding to make a tuple
mkTupleE :: [Exp] -> Exp
mkTupleE = Lam (mkIdent "$f") . foldl App (Var (mkIdent "$f"))

-- Select component m from an n-tuple
mkTupleSelE :: Int -> Int -> Exp -> Exp
mkTupleSelE m n tup =
  let
    xs = [mkIdent ("x" ++ show i) | i <- [1 .. n] ]
  in App tup (foldr Lam (Var (xs !! m)) xs)

-- Handle special syntax for lists and tuples
dsPat :: --XHasCallStack =>
         EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EListish (LList ps) -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (getSLoc ap) (length ps)) ps
    EAt i p -> EAt i (dsPat p)
    ELit loc (LStr cs) | length cs < 2 -> dsPat (EListish (LList (map (ELit loc . LChar) cs)))
    ELit _ _ -> ap
    _ -> impossible

iNil :: Ident
iNil = mkIdent $ listPrefix ++ "[]"

iCons :: Ident
iCons = mkIdent $ listPrefix ++ ":"

consCon :: EPat
consCon = ECon $ ConData [(iNil, 0), (iCons, 2)] iCons

nilCon :: EPat
nilCon = ECon $ ConData [(iNil, 0), (iCons, 2)] iNil

tupleCon :: SLoc -> Int -> EPat
tupleCon loc n =
  let
    c = tupleConstr loc n
  in ECon $ ConData [(c, n)] c

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: String -> [Ident] -> [Ident]
newVars s is = deleteAllsBy (==) [ mkIdent (s ++ show i) | i <- [1::Int ..] ] is

newVar :: [Ident] -> Ident
newVar = head . newVars "$q"

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> showIdent i ++ " = " ++ show e

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
--showArm (ps, _, b) = showListS showExpr ps ++ "," ++ show b

newIdents :: Int -> M [Ident]
newIdents n = do
  is <- get
  put (drop n is)
  return (take n is)

newIdent :: M Ident
newIdent = do
  is <- get
  put (tail is)
  return (head is)

runS :: SLoc -> [Ident] -> [Exp] -> Matrix -> Exp
runS loc used ss mtrx =
  let
    supply = newVars "$x" used
    ds xs aes =
      case aes of
        []   -> dsMatrix (eMatchErr loc) (reverse xs) mtrx
        e:es -> letBind (return e) $ \ x -> ds (x:xs) es
  in evalState (ds [] ss) supply

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
   return dflt
 else
 case iis of
 [] -> let { (_, f, _) : _ = aarms } in return $ f dflt
 i:is -> do
  let
    (arms, darms, rarms) = splitArms aarms
    ndarms = map (\ (EVar x : ps, ed, g) -> (ps, substAlpha x i . ed, g) ) darms
--  traceM ("split " ++ show (arms, darms, rarms))
  letBind (dsMatrix dflt iis rarms) $ \ drest ->
    letBind (dsMatrix drest is ndarms) $ \ ndflt ->
     if null arms then
       return ndflt
     else do
      let
        idOf (p:_, _, _) = pConOf p
        idOf _ = impossible
        grps = groupEq (on (==) idOf) arms
        oneGroup grp = do
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
          return (SPat con xs, cexp)
--      traceM $ "grps " ++ show grps
      narms <- mapM oneGroup grps
      return $ mkCase i narms ndflt

eMatchErr :: SLoc -> Exp
eMatchErr (SLoc fn l c) =
  App (App (App (Lit (LPrim "noMatch")) (Lit (LStr fn))) (Lit (LInt l))) (Lit (LInt c))

-- If the first expression isn't a variable/literal, then use
-- a let binding and pass variable to f.
letBind :: M Exp -> (Exp -> M Exp) -> M Exp
letBind me f = do
  e <- me
  if cheap e then
    f e
   else do
    x <- newIdent
    r <- f (Var x)
    return $ eLet x e r

cheap :: Exp -> Bool
cheap ae =
  case ae of
    Var _ -> True
    Lit _ -> True
    _ -> False

eEqInt :: Exp
eEqInt = Lit (LPrim "==")

eEqChar :: Exp
eEqChar = Lit (LPrim "==")

eEqStr :: Exp
eEqStr = Lit (LPrim "equal")

mkCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
mkCase var pes dflt =
  --trace ("mkCase " ++ show pes) $
  case pes of
    [] -> dflt
    [(SPat (ConNew _) [x], arhs)] -> eLet x var arhs
    (SPat (ConLit _ l) _,   arhs) : rpes -> 
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
            (vs, rhs) = head $ [ (xs, e) | (SPat (ConData _ i) xs, e) <- pes, c == i ] ++
                               [ (replicate k dummyIdent, dflt) ]
          in (SPat (ConData cs c) vs, rhs)
      in  eCase var (map arm cs)
    _ -> impossible

eCase :: Exp -> [(SPat, Exp)] -> Exp
eCase e as =
--  trace ("eCase " ++ show e ++ "\n" ++
--         unlines [ unwords (map showIdent (conIdent c : xs)) ++ " -> " ++ show r | (SPat c xs, r) <- as ]) $
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
  if x == dummyIdent then
    e
  else
    substExp x y e

eLet :: Ident -> Exp -> Exp -> Exp
eLet i e b =
  if i == dummyIdent then
    b
  else
    case b of
      Var j | i == j -> e
      _ ->
        case filter (== i) (freeVars b) of
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
    ELit loc l -> ConLit loc l
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
  case getDups (==) (filter (/= dummyIdent) $ map fst ds) of
    [] -> ds
    (i1:_i2:_) : _ ->
      errorMessage (getSLoc i1) $ "duplicate definition " ++ showIdent i1
        -- XXX mysteriously the location for i2 is the same as i1
        -- ++ ", also at " ++ showSLoc (getSLoc i2)
    _ -> error "checkDup"

-- Make recursive definitions lazier.
-- The idea is that we have
--  f x y = ... (f x) ...
-- we turn this into
--  f x = letrec f' y = ... f' ... in f'
-- thus avoiding the extra argument passing.
-- XXX should generalize for an arbitrary length prefix of variables.
-- This gives a small speedup with overloading.
lazier :: LDef -> LDef
lazier def@(fcn, Lam x (Lam y body)) =
  let fcn' = addIdentSuffix fcn "@"
      vfcn' = Var fcn'
      repl :: Exp -> State Bool Exp
      repl (Lam i e) = Lam i <$> repl e
      repl (App (Var af) (Var ax)) | af == fcn && ax == x = do
        put True
        return vfcn'
      repl (App f a) = App <$> repl f <*> repl a
      repl e@(Var _) = return e
      repl e@(Lit _) = return e
  in  case runState (repl body) False of
        (_, False) -> def
        (e', True) -> (fcn, Lam x $ letRecE fcn' (Lam y e') vfcn')

lazier def = def
