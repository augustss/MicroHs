-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
{-# LANGUAGE QualifiedDo #-}
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
--import Debug.Trace

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
    ECaseS e as -> apps (dsExpr e) [lams xs $ dsExpr r | (SPat _ xs, r) <- as ]

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

{-
dsCase :: Expr -> [ECaseArm] -> Exp
dsCase ecase aarms =
  case findDefault [(dsPat p, e) | (p, e) <- aarms] of
    (arms, dexpr) ->
      case arms of
        -- No pattern matching
        [] -> App (dsExpr dexpr) (dsExpr ecase)
        (pat, _) : _ ->
          let
            cs = getConTyInfo pat
            nvs = newVars (allVarsExpr (ECase ecase aarms))
          in  dsCaseArms nvs cs ecase arms dexpr
-}

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

{-
getConTyInfo :: --XHasCallStack =>
                EPat -> ConTyInfo
getConTyInfo ap =
  case ap of
    ECon c -> conTyInfo c
    EApp f _ -> getConTyInfo f
    _ -> impossible

getSubPats :: EPat -> [EPat]
getSubPats =
  let
    getsp ps ap =
      case ap of
        ECon _ -> ps
        EApp f p -> getsp (p:ps) f
        _ -> impossible
  in getsp []

getConName :: EPat -> Ident
getConName ap =
  case ap of
    ECon c -> conIdent c
    EApp f _ -> getConName f
    _ -> impossible
-}

dummyIdent :: Ident
dummyIdent = "_"

eError :: String -> Expr
eError s = EApp (EPrim "error") (EStr s)

{-
findDefault :: [ECaseArm] -> ([ECaseArm], Expr)
findDefault aarms =
  case aarms of
    [] -> ([], ELam dummyIdent $ eError "no match")
    arm : arms ->
      case arm of
         (pat, rhs) ->
           case pat of
             EVar i -> ([], ELam i rhs)
             _ ->
               case findDefault arms of
                 (narms, dflt) -> (arm : narms, dflt)

dsCaseArms :: [Ident] -> [(Ident, Int)] -> Expr -> [ECaseArm] -> Expr -> Exp
--dsCaseArms nvs cons e arms dflt | trace (show (arms, dflt)) False = undefined
dsCaseArms nvs cons e arms dflt =
  let
    nv = head nvs
    (ev, elet) = asVar nv (dsExpr e)
    --(ed, dlet) = asVar (head (tail nvs)) $ App (dsExpr dflt) (Var ev)
    edflt = EApp dflt (EVar ev)

    rarms = reorderArms cons arms edflt

    dsArm aarm =
      case aarm of
        (apat, rhs) ->
          let
            ps = getSubPats apat
            vs = take (length ps) (tail nvs)
            pat vp r =
              case vp of
                (v, p) -> ECase (EVar v) [(p, r), (EVar dummyIdent, if length arms == 1 then edflt else EBad (showEPat p))]
            cr = foldr pat rhs (zip vs ps)
          in lams vs $ dsExpr cr

  in elet $ apps (Var ev) (map dsArm rarms)

asVar :: Ident -> Exp -> (Ident, Exp -> Exp)
asVar i ae =
  case ae of
    Var ii -> (ii, id)
    _ -> (i, \ e -> App (Lam i e) ae)

reorderArms :: [(Ident, Int)] -> [ECaseArm] -> Expr -> [ECaseArm]
--reorderArms cs as ed | trace (show (cs, as, ed)) False = undefined
reorderArms cons as ed =
  let
    npats = length . getSubPats
    arms = map (\ a -> (getConName (fst a), a)) as
    arm ck =
      case ck of
        (c, k) ->
          case lookupBy eqIdent c arms of
            Nothing -> (foldl EApp (ECon $ Con cons c) (replicate k (EVar dummyIdent)), ed)
            Just a ->
              if npats (fst a) == k then a else error $ "bad contructor arity: " ++ showIdent c
  in  map arm cons
-}

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: [Ident] -> [Ident]
newVars is = deleteFirstsBy eqIdent [ "nv" ++ showInt i | i <- enumFrom 1 ] is

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
    r = runS (allVarsExpr (ECase ae as)) [ae] [([dsPat p], e) | (p, e) <- as]
  in --trace (showExpr r) $
     dsExpr r
     

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Expr)
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

runS :: [Ident] -> [Expr] -> Matrix -> Expr
runS used ss mtrx =
  let
    supply = (deleteFirstsBy eqIdent [ "nv" ++ showInt i | i <- enumFrom 1 ] used)
    ds xs aes =
      case aes of
        []   -> --letBind (S.return eMatchErr) $ \ d ->
                dsMatrix eMatchErr (reverse xs) mtrx
        e:es -> letBind (S.return e) $ \ x -> ds (x:xs) es
  in S.evalState (ds [] ss) supply

-- Desugar a pattern matrix.
-- The input is a (usually identifier) vector e1, ..., en
-- and patterns matrix p11, ..., p1n   -> e1
--                     p21, ..., p2n
--                     pm1, ..., pmn   -> em
-- The output is an expressions where each case expressions
-- only has simple matching, i.e., case e { C1 v11 ... v1n -> e1; ...; _ -> ed }
dsMatrix :: Expr -> [Expr] -> Matrix -> M Expr
{-
dsMatrix d _ [] = S.return (EVar d)
dsMatrix _ [] ((_,e):_) = S.return e
--dsMatrix _  _ [] = S.return eMatchErr
dsMatrix dflt iis@(i:is) aarms = S.do
-}
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
          cexp <- dsMatrix ndflt (map EVar xs ++ is) (map one grp)
          S.return (SPat con xs, cexp)
--      traceM $ "grps " ++ show grps
      narms <- S.mapM oneGroup grps
      S.return $ mkCase i narms ndflt

eMatchErr :: Expr
eMatchErr = EApp (EPrim "error") (EStr "no match")

-- If the first expression isn't a variable, the use
-- a let binding and pass variable to f.
letBind :: M Expr -> (Expr -> M Expr) -> M Expr
letBind me f = S.do
  e <- me
  if cheap e then
    f e
   else S.do
    x <- newIdent
    r <- f (EVar x)
    S.return $ eLet x e r

cheap :: Expr -> Bool
cheap ae =
  case ae of
    EVar _ -> True
    ECon _ -> True
    EInt _ -> True
    EChar _ -> True
    EStr _ -> True
    EApp f _ ->
      case f of
        EPrim _ -> True
        _ -> False
    _ -> False

mkCase :: Expr -> [(SPat, Expr)] -> Expr -> Expr
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
  in  ECaseS var (map arm cs)

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
substAlpha :: Ident -> Expr -> Expr -> Expr
substAlpha x y e = eLet x y e
--  subst [(x, y)] e

eLet :: Ident -> Expr -> Expr -> Expr
eLet i e b =
  if eqIdent i dummyIdent then
    b
  else
    let
      r = ELet [(BFcn (i,[]) e)] b
    in case b of
         EVar j -> if eqIdent i j then e else r
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
