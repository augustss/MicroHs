-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE QualifiedDo #-}
module MicroHs.Desugar(
  module MicroHs.Desugar
  --desugar, LDef, showLDefs,
  {-
  desugar,
  Module(..),
  Export,
  TypeDef(..),
  LDef,
  SymTable,
  TypeTable,
-}
  ) where
--import Debug.Trace
import Prelude
import Data.Char
import Data.List
import Control.Monad.State.Strict as S
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
    ECon _ i ->
      if eqChar (head i) ',' then
        undefined  -- not implemented yet
      else
        Var i

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

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

-- Handle special syntax for lists and tuples
dsPat :: EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EList ps -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (length ps)) ps
    _ -> impossible

consCon :: EPat
consCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon [(n, 0), (c, 2)] c

nilCon :: EPat
nilCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon [(n, 0), (c, 2)] n

tupleCon :: Int -> EPat
tupleCon n =
  let
    c = tupleConstr n
  in ECon [(c, n)] c

getConTyInfo :: --XHasCallStack =>
                EPat -> ConTyInfo
getConTyInfo ap =
  case ap of
    ECon cs _ -> cs
    EApp f _ -> getConTyInfo f
    _ -> impossible

getSubPats :: EPat -> [EPat]
getSubPats =
  let
    get ps ap =
      case ap of
        ECon _ _ -> ps
        EApp f p -> get (p:ps) f
        _ -> impossible
  in get []

getConName :: EPat -> Ident
getConName ap =
  case ap of
    ECon _ c -> c
    EApp f _ -> getConName f
    _ -> impossible

dummyIdent :: Ident
dummyIdent = "_"

eError :: String -> Expr
eError s = EApp (EPrim "error") (EStr s)

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
    conArity = length . getSubPats
    arms = map (\ a -> (getConName (fst a), a)) as
    arm ck =
      case ck of
        (c, k) ->
          case lookupBy eqIdent c arms of
            Nothing -> (foldl EApp (ECon cons c) (replicate k (EVar dummyIdent)), ed)
            Just a ->
              if conArity (fst a) == k then a else error $ "bad contructor arity: " ++ showIdent c
  in  map arm cons

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

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Expr)
type Matrix = [Arm]

-- Desugar a pattern matrix.
-- The input is an identifier vector vector i1, ..., en
-- and patterns matrix p11, ..., p1n   -> e1
--                     p21, ..., p2n
--                     pm1, ..., pmn   -> em
-- The output is an expressions where each case expressions
-- only has simple matching, i.e., case e { C1 v11 ... v1n -> e1; ...; _ -> ed }
dsMatrix :: Expr -> [Ident] -> Matrix -> M Expr
dsMatrix d [] [] = d
dsMatrix _ [] ((_,e):_) = S.return e
dsMatrix _  _ [] = S.return eMatchErr
dsMatrix d iis@(i:is) aarms = S.do
  let
    -- XXX handle EAt
    (arms, darms, rarms) = splitArms aarms
    eRest = dsMatrix d iis rarms
    ndarms = map (\ (PVar x : ps, ed) -> (ps, substAlpha x i ed)) darms
    ndflt = dsMatrix d is ndarms
    grps = groupEq (on leIdent (conIdent . pConOf . head . fst)) arms
    oneGroup grp = S.do
      let
        (pat:_, _) : _ = grp
        con = pConOf pat
      xs <- mapM (const getIdent) (enumFrom 1 (conArity con))
      let
        cpat = foldl EApp con (map EVar xs)
        cexp = dsMatrix ndflt (xs ++ is) (map (\ (p:ps, e) -> pArgs p ++ ps, e) grp)
      S.return (cpat, cexp)
  narms <- mapM oneGroup grps
  S.return $ ECase (EVar i) narms

eMatchErr :: Expr
eMatchErr = EApp (EPrim "error") (EStr "no match")

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
substAlpha :: Ident -> Ident -> Expr -> Expr
substAlpha x y e = ELet [BFcn (x,[]) (EVar y)] e

pConOf :: EPat -> EPat
pConOf p@(ECon _ _) = p
pConOf (EAt _ p) = pConOf p
pConOf (EApp p _) = pConOf p
pConOf _ = impossible

pArgs :: EPat -> [EPat]
pArgs (ECon _ _) = []
pArgs (EAp _ p) = pArgs p
pArgs (EApp f a) = pArgs f ++ [a]
pArgs _ = impossible

conIdent :: EPat -> Ident
conIdent (ECon _ i) = i
conIdent _ = impossible

conArity :: EPat -> Int
conArity (ECon cs i) = fromMaybe impossible $ lookupBy eqIdent i cs

-- XXX quadratic
groupEq :: (a -> a -> Bool) -> [a] -> [[a]]
groupEq eq [] = []
groupEq eq (x:xs) =
  let
    (es, ns) = partition (eq x) xs
  in (x:es) : groupEq eq ns
