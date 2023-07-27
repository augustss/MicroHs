-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
module MicroHs.Desugar(
  module MicroHs.Desugar
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
import qualified MicroHs.StringMap as M
import Data.Maybe
--Ximport Compat
--import Debug.Trace

import MicroHs.Parse
import MicroHs.Exp

data Module = Module IdentModule [Export] [TypeDef] [LDef]
  --Xderiving (Show)

type Export = (Ident, Ident)  -- exported name, global name

data TypeDef = TypeDef Ident [(Ident, Int)]   -- constructor name, arity
  --Xderiving (Show, Eq)

type LDef = (Ident, Exp)

type ValTable = M.Map [Exp]
type TypeTable = M.Map [TypeDef]

data SymTable = SymTable ValTable TypeTable
  --Xderiving (Show)

sVal :: SymTable -> ValTable
sVal ast =
  case ast of
    SymTable vt _ -> vt

sType :: SymTable -> TypeTable
sType ast =
  case ast of
    SymTable _ tt -> tt

eqTypeDef :: TypeDef -> TypeDef -> Bool
eqTypeDef at1 at2 =
  case at1 of
    TypeDef i1 iis1 ->
      case at2 of
        TypeDef i2 iis2 ->
          eqIdent i1 i2 && eqList (eqPair eqIdent eqInt) iis1 iis2

extVals :: SymTable -> [Ident] -> SymTable
extVals sym is =
  case sym of
    SymTable vals tys -> SymTable (foldr (\ x -> M.insert x [Var x]) vals is) tys

extVal :: SymTable -> Ident -> SymTable
extVal sym x =
  case sym of
    SymTable vals tys -> SymTable (M.insert x [Var x] vals) tys

mkSymTable :: [(ImportSpec, Module)] -> SymTable
mkSymTable mdls =
  let
    qns aisp mn i =
      case aisp of
        ImportSpec q _ mas ->
          let
            m = fromMaybe mn mas
          in  if q then [qual m i] else [i, qual m i]
    --XallVals :: ValTable
    allVals =
      let
        syms arg =
          case arg of
            (is, Module mn qis _ _) -> [ (v, [Var qi]) | (i, qi) <- qis, v <- qns is mn i ]
      in  M.fromListWith (unionBy eqExp) $ concatMap syms mdls
    --XallTypes :: TypeTable
    allTypes =
      let
        types arg =
          case arg of
            (is, Module mn _ tds _) ->
              [ (v, [td]) | td <- tds, let { TypeDef _ cs = td }, (c, _) <- cs, v <- qns is mn c ]
      in M.fromListWith (unionBy eqTypeDef) $ concatMap types mdls
  in  SymTable allVals allTypes

-- Combine two symbol table, first one has shadows second one.
unionSymTable :: SymTable -> SymTable -> SymTable
unionSymTable ast1 ast2 =
  case ast1 of
    SymTable lv lt ->
      case ast2 of
        SymTable rv rt -> SymTable (M.union lv rv) (M.union lt rt)

desugar :: [(ImportSpec, Module)] -> EModule -> Module
desugar imdls amdl =
  case amdl of
    EModule mdln especs ds ->
      let
        mdl_allSyms =
          let
            allSyms = snd mdl_allSyms
            dsd = concatMap (dsDef allSyms) ds
            exportT ex =
              case ex of
                ExpModule m ->
                  if eqIdent m mdln then
                    concatMap dsData ds
                  else
                    [ td | (_, Module mn _ tds _) <- imdls, eqIdent mn m, td <- tds ]
            tyds = concatMap exportT especs
            exportD ex =
              case ex of
                ExpModule m ->
                  if eqIdent m mdln then
                    [(i, qual mdln i) | (i, _) <- dsd]
                  else
                    [ e | (_, Module mn es _ _) <- imdls, eqIdent mn m, e <- es ]
            exps = concatMap exportD especs
            mdl = Module mdln exps tyds [(qual mdln i, e) | (i, e) <- dsd]
            impSyms = mkSymTable imdls
            thisSyms = mkSymTable [(ImportSpec False mdln Nothing, mdl)]
            allSymsR = unionSymTable thisSyms impSyms
          in (mdl, allSymsR)
      in  fst mdl_allSyms

dsDef :: SymTable -> EDef -> [LDef]
dsDef syms adef =
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
              in (c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      in  zipWith dsConstr (enumFrom 0) cs
    Type _ _ -> []
    Fcn fxs e ->
      case fxs of
        (f, xs) -> [(f, lams xs $ dsExpr (extVals syms xs) e)]
    Sign _ _ -> []
    Import _ -> []

dsBind :: SymTable -> EBind -> [LDef]
dsBind syms abind =
  case abind of
    BFcn (f, xs) e -> [(f, lams xs $ dsExpr (extVals syms xs) e)]
    BPat p e ->
      let
        v = newVar (allVarsBind abind)
        de = (v, dsExpr syms e)
        ds = [ (i, dsExpr syms (ECase (EVar v) [(p, EVar i)])) | i <- patVars p ]
      in  de : ds

dsExpr :: SymTable -> Expr -> Exp
dsExpr syms aexpr =
  case aexpr of
    EVar i ->
      case M.lookup i (sVal syms) of
        Nothing -> error $ "undefined: " ++ showIdent i
        Just qis ->
          if length qis == 1 then head qis
          else error $ "ambiguous: " ++ showIdent i -- ++ ", " ++ show qis
    EApp f a -> App (dsExpr syms f) (dsExpr syms a)
    ELam x e -> Lam x (dsExpr (extVal syms x) e)
    EInt i -> Int i
    EChar c -> Int (ord c)
    ECase e as -> dsCase syms e as
-- For now, just sequential bindings; each recursive
    ELet ads e ->
      case ads of
        [] -> dsExpr syms e
        d:ds ->
          let
            dsd_nsyms =
              (dsBind (snd dsd_nsyms) d, extVals syms (map fst (fst dsd_nsyms)))
            dsd = fst dsd_nsyms
            nsyms = snd dsd_nsyms
            de = dsExpr nsyms (ELet ds e)
            def ir a =
                case ir of
                  (i, r) -> App (Lam i a) (App (Prim "Y") (Lam i r))
          in  foldr def de dsd
    EList es ->
      foldr (app2 cCons) cNil $ map (dsExpr syms) es
    ETuple es -> Lam "_f" $ foldl App (Var "_f") $ map (dsExpr syms) es
    EStr cs -> dsExpr syms $ EList $ map EChar cs
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
                in  dsExpr syms $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam nv body)
            SThen e ->
              if null stmts then
                dsExpr syms e
              else
                dsExpr syms $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn stmts)
            SLet ds ->
              if null stmts then error "do without final expression" else
                dsExpr syms $ ELet ds (EDo mn stmts)

    EPrim s -> Prim s
    ESectL e op ->
      App (dsExpr syms (EVar op)) (dsExpr syms e)
    ESectR op e ->
      app2 cFlip (dsExpr syms (EVar op)) (dsExpr syms e)
    EIf e1 e2 e3 ->
      app2 (dsExpr syms e1) (dsExpr syms e3) (dsExpr syms e2)
    ECompr e astmts ->
      case astmts of
        [] -> dsExpr syms (EList [e])
        stmt : stmts ->
          case stmt of
            SBind p b ->
              let
                nv = newVar (allVarsExpr aexpr)
                body = ECase (EVar nv) [(p, ECompr e stmts), (PVar dummyIdent, EList [])]
              in app2 (Var "Data.List.concatMap") (dsExpr syms (ELam nv body)) (dsExpr syms b)
            SThen c ->
              dsExpr syms (EIf c (ECompr e stmts) (EList []))
            SLet ds ->
              dsExpr syms (ELet ds (ECompr e stmts))
    EBad -> error "complex case not implemented"
    EUVar _ -> undefined
    ECaseT _ _ _ -> undefined

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

dsCase :: SymTable -> Expr -> [ECaseArm] -> Exp
dsCase syms ecase aarms =
  case findDefault aarms of
    (arms, dexpr) ->
      case arms of
        -- No pattern matching
        [] -> App (dsExpr syms dexpr) (dsExpr syms ecase)
        (pat, _) : _ ->
          case pat of
            PVar _ -> undefined  -- impossible
            PConstr con _ ->
              case constrType con (sType syms) of
                TypeDef _ cs ->
                  let
                    nvs = newVars (allVarsExpr (ECase ecase aarms))
                  in  dsCaseArms syms nvs cs ecase arms dexpr

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
             PConstr _ _ ->
               case findDefault arms of
                 (narms, dflt) -> (arm : narms, dflt)

dsCaseArms :: SymTable -> [Ident] -> [(Ident, Int)] -> Expr -> [ECaseArm] -> Expr -> Exp
dsCaseArms syms nvs constrs e arms dflt =
  let
    nv = head nvs
    (ev, elet) = asVar nv (dsExpr syms e)
    --(ed, dlet) = asVar (head (tail nvs)) $ App (dsExpr syms dflt) (Var ev)
    edflt = EApp dflt (EVar ev)

    rarms = reorderArms constrs arms edflt

    dsArm aarm =
      case aarm of
        (apat, rhs) ->
          case apat of
            PVar _ -> undefined -- impossible
            PConstr _ ps ->
              let
                vs = take (length ps) (tail nvs)
                pat vp r =
                  case vp of
                    (v, p) -> ECase (EVar v) [(p, r), (PVar dummyIdent, EBad)]
                cr = foldr pat rhs (zip vs ps)
              in lams vs $ dsExpr (extVals syms (ev:vs)) cr

  in elet $ apps (Var ev) (map dsArm rarms)

asVar :: Ident -> Exp -> (Ident, Exp -> Exp)
asVar i ae =
  case ae of
    Var ii -> (ii, id)
    _ -> (i, \ e -> App (Lam i e) ae)

reorderArms :: [(Ident, Int)] -> [ECaseArm] -> Expr -> [ECaseArm]
reorderArms constrs as ed =
  let
    conName arg =
      case arg of
        PConstr c _ -> c
        _ -> undefined
    conArity arg =
      case arg of
        PConstr _ ps -> length ps
        _ -> undefined
    arms = map (\ a -> (conName (fst a), a)) as
    arm ck =
      case ck of
        (c, k) ->
          case lookupBy eqIdent c arms of
            Nothing -> (PConstr c (replicate k (PVar dummyIdent)), ed)
            Just a ->
              if conArity (fst a) == k then a else error $ "bad contructor arity: " ++ showIdent c
  in  map arm constrs

constrType :: Ident -> TypeTable -> TypeDef
constrType con tys =
  if eqString (take 1 con) "," then
    TypeDef con [(con, untupleConstr con)]
  else
    case M.lookup con tys of
      Nothing -> error $ "undefined constructor: " ++ showIdent con
      Just tds ->
        if length tds == 1 then head tds else error $ "ambiguous constructor: " ++ showIdent con

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

dsData :: EDef -> [TypeDef]
dsData def =
  case def of
    Data (tn, _) cs -> [TypeDef tn [(c, length ts) | (c, ts) <- cs ]]
    _ -> []

patVars :: EPat -> [Ident]
patVars apat =
  case apat of
    PVar i -> [i]
    PConstr _ ps -> concatMap patVars ps

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
    PConstr i ps -> i : concatMap allVarsPat ps
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
    ECaseT _ e as -> allVarsExpr e ++ concatMap (\ pa -> allVarsPat (fst pa) ++ allVarsExpr (snd pa)) as

allVarsStmt :: EStmt -> [Ident]
allVarsStmt astmt =
  case astmt of
    SBind p e -> allVarsPat p ++ allVarsExpr e
    SThen e -> allVarsExpr e
    SLet bs -> concatMap allVarsBind bs

dsType :: EType -> EType
dsType ea =
  case ea of
    EVar _ -> ea
    EApp f a -> EApp (dsType f) (dsType a)
    ETuple ts -> foldl EApp (EVar (tupleConstr (length ts))) (map dsType ts)
    EList ts ->
      case ts of
        [] -> EVar "[]"
        t : _ -> EApp (EVar "[]") (dsType t)
    _ -> undefined
