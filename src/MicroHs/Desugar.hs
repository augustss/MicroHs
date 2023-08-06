-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
module MicroHs.Desugar(
  --module MicroHs.Desugar
  desugar, LDef, showLDefs,
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
--Ximport Compat
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
                  body = ECase (EVar nv) [(p, EDo mn stmts), (PVar dummyIdent, eError "dopat")]
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
                body = ECase (EVar nv) [(p, ECompr e stmts), (PVar dummyIdent, EList [])]
              in app2 (Var "Data.List.concatMap") (dsExpr (ELam nv body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (ECompr e stmts) (EList []))
            SLet ds ->
              dsExpr (ELet ds (ECompr e stmts))
    EBad -> error "complex case not implemented"
    EUVar _ -> undefined
    ECon _ i -> Var i

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

dsCase :: Expr -> [ECaseArm] -> Exp
dsCase ecase aarms =
  case findDefault aarms of
    (arms, dexpr) ->
      case arms of
        -- No pattern matching
        [] -> App (dsExpr dexpr) (dsExpr ecase)
        (pat, _) : _ ->
          case pat of
            PVar _ -> undefined  -- impossible
            PConstr cs _ _ ->
              let
                nvs = newVars (allVarsExpr (ECase ecase aarms))
              in  dsCaseArms nvs cs ecase arms dexpr

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
             PVar i -> ([], ELam i rhs)
             PConstr _ _ _ ->
               case findDefault arms of
                 (narms, dflt) -> (arm : narms, dflt)

dsCaseArms :: [Ident] -> [(Ident, Int)] -> Expr -> [ECaseArm] -> Expr -> Exp
dsCaseArms nvs constrs e arms dflt =
  let
    nv = head nvs
    (ev, elet) = asVar nv (dsExpr e)
    --(ed, dlet) = asVar (head (tail nvs)) $ App (dsExpr dflt) (Var ev)
    edflt = EApp dflt (EVar ev)

    rarms = reorderArms constrs arms edflt

    dsArm aarm =
      case aarm of
        (apat, rhs) ->
          case apat of
            PVar _ -> undefined -- impossible
            PConstr _ _ ps ->
              let
                vs = take (length ps) (tail nvs)
                pat vp r =
                  case vp of
                    (v, p) -> ECase (EVar v) [(p, r), (PVar dummyIdent, EBad)]
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
reorderArms constrs as ed =
  let
    conName arg =
      case arg of
        PConstr _ c _ -> c
        _ -> undefined
    conArity arg =
      case arg of
        PConstr _ _ ps -> length ps
        _ -> undefined
    arms = map (\ a -> (conName (fst a), a)) as
    arm ck =
      case ck of
        (c, k) ->
          case lookupBy eqIdent c arms of
            Nothing -> (PConstr constrs c (replicate k (PVar dummyIdent)), ed)
            Just a ->
              if conArity (fst a) == k then a else error $ "bad contructor arity: " ++ showIdent c
  in  map arm constrs

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: [Ident] -> [Ident]
newVars is = deleteFirstsBy eqIdent [ "nv" ++ showInt i | i <- enumFrom 1 ] is

newVar :: [Ident] -> Ident
newVar = head . newVars

allVarsBind :: EBind -> [Ident]
allVarsBind abind =
  case abind of
    BFcn l e -> allVarsLHS l ++ allVarsExpr e
    BPat p e -> allVarsPat p ++ allVarsExpr e

allVarsLHS :: LHS -> [Ident]
allVarsLHS iis =
  case iis of
    (i, is) -> i : is

allVarsPat :: EPat -> [Ident]
allVarsPat apat =
  case apat of
    PConstr _ i ps -> i : concatMap allVarsPat ps
    PVar i -> [i]

allVarsExpr :: Expr -> [Ident]
allVarsExpr aexpr =
  case aexpr of
    EVar i -> [i]
    EApp e1 e2 -> allVarsExpr e1 ++ allVarsExpr e2
    ELam i e -> i : allVarsExpr e
    EInt _ -> []
    EChar _ -> []
    EStr _ -> []
    ECase e as -> allVarsExpr e ++ concatMap (\ pa -> allVarsPat (fst pa) ++ allVarsExpr (snd pa)) as
    ELet bs e -> concatMap allVarsBind bs ++ allVarsExpr e
    ETuple es -> concatMap allVarsExpr es
    EList es -> concatMap allVarsExpr es
    EDo mi ss -> maybe [] (:[]) mi ++ concatMap allVarsStmt ss
    EPrim _ -> []
    ESectL e i -> i : allVarsExpr e
    ESectR i e -> i : allVarsExpr e
    EIf e1 e2 e3 -> allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3
    ECompr e ss -> allVarsExpr e ++ concatMap allVarsStmt ss
    EBad -> []
    EUVar _ -> []
    ECon _ i -> [i]

allVarsStmt :: EStmt -> [Ident]
allVarsStmt astmt =
  case astmt of
    SBind p e -> allVarsPat p ++ allVarsExpr e
    SThen e -> allVarsExpr e
    SLet bs -> concatMap allVarsBind bs

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> i ++ " = " ++ showExp e
