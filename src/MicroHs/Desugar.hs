-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults #-}
module MicroHs.Desugar(
  desugar,
  Module(..),
  Export,
  TypeDef(..),
  LDef,
  SymTable,
  TypeTable,
  ) where
--import Debug.Trace
import Data.List
import qualified Data.Map as M
import Data.Maybe

import MicroHs.Parse
import MicroHs.Exp

data Module = Module IdentModule [Export] [TypeDef] [LDef]
  deriving (Show)

type Export = (Ident, Ident)  -- exported name, global name

data TypeDef = TypeDef Ident [(Ident, Int)]   -- constructor name, arity
  deriving (Show, Eq)

type LDef = (Ident, Exp)

type ValTable = M.Map Ident [Exp]
type TypeTable = M.Map Ident [TypeDef]

data SymTable = SymTable { sVal :: ValTable, sType :: TypeTable }
  deriving (Show)

extVals :: SymTable -> [Ident] -> SymTable
extVals (SymTable vals tys) is = SymTable (foldr (\ x -> M.insert x [Var x]) vals is) tys

mkSymTable :: [(ImportSpec, Module)] -> SymTable
mkSymTable mdls =
  let
    qns (ImportSpec q _ mas) mn i =
      let mn' = fromMaybe mn mas
      in  if q then [qual mn' i] else [i, qual mn' i]
    allVals :: ValTable
    allVals = M.fromListWith union $ concatMap syms mdls
      where syms (is, Module mn qis _ _) = [ (v, [Var qi]) | (i, qi) <- qis, v <- qns is mn i ]
    allTypes :: TypeTable
    allTypes = M.fromListWith union $ concatMap types mdls
      where types (is, Module mn _ tds _) = [ (v, [td]) | td@(TypeDef _ cs) <- tds, (c, _) <- cs, v <- qns is mn c ]
  in  SymTable allVals allTypes

-- Combine two symbol table, first one has shadows second one.
unionSymTable :: SymTable -> SymTable -> SymTable
unionSymTable (SymTable lv lt) (SymTable rv rt) = SymTable (M.union lv rv) (M.union lt rt)

desugar :: [(ImportSpec, Module)] -> EModule -> Module
desugar imdls (EModule mdln especs ds) =
  let ds' = concatMap (dsDef allSyms) ds
      tyds = concatMap exportT especs
      exportT (ExpModule m) =
        if m == mdln then
          concatMap dsData ds
        else
          [ td | (_, Module mn _ tds _) <- imdls, mn == m, td <- tds ]
      exps = concatMap exportD especs
      exportD (ExpModule m) =
        if m == mdln then
          [(i, qual mdln i) | (i, _) <- ds']
        else
          [ e | (_, Module mn es _ _) <- imdls, mn == m, e <- es ]
      mdl = Module mdln exps tyds [(qual mdln i, e) | (i, e) <- ds']
      impSyms = mkSymTable imdls
      thisSyms = mkSymTable [(ImportSpec False mdln Nothing, mdl)]
      allSyms = unionSymTable thisSyms impSyms
  in  mdl

dsDef :: SymTable -> EDef -> [LDef]
dsDef _ (Data _ cs) = zipWith dsConstr [0..] cs
  where
    fs = [f i | (i, _) <- zip [0..] cs]
    dsConstr i (c, ts) = (c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      where xs = ["$x" ++ show (j::Int) | (j, _) <- zip [0..] ts]
    f i = "$f" ++ show (i::Int)
dsDef _ Type{} = []
dsDef syms (Fcn (f, xs) e) = [(f, lams xs $ dsExpr (extVals syms xs) e)]
dsDef _ Sign{} = []
dsDef _ Import{} = []

dsBind :: SymTable -> EBind -> [LDef]
dsBind syms (BFcn (f, xs) e) = [(f, lams xs $ dsExpr (extVals syms xs) e)]
dsBind syms b@(BPat p e) =
  let v = newVar (allVarsBind b)
      de = (v, dsExpr syms e)
      ds = [ (i, dsExpr syms (ECase (EVar v) [(p, EVar i)])) | i <- patVars p ]
  in  de : ds

dsExpr :: SymTable -> Expr -> Exp
dsExpr syms (EVar i) =
  case M.lookup i (sVal syms) of
    Nothing -> error $ "undefined: " ++ show i
    Just [qi] -> qi
    Just qis -> error $ "ambiguous: " ++ show i ++ ", " ++ show qis
dsExpr syms (EApp f a) = App (dsExpr syms f) (dsExpr syms a)
dsExpr syms (ELam xs e) = lams xs (dsExpr (extVals syms xs) e)
dsExpr _ (EInt i) = Int i
dsExpr _ (EChar c) = Int (fromEnum c)
dsExpr syms (ECase e as) = dsCase syms e as
{-
dsExpr syms (ECase e as) = apps (dsExpr syms e) (map dsArm as')
  where dsArm (PConstr _ xxxvs, r) = lams vs $ dsExpr (extVals syms vs) r
          where vs = map (\ (PVar v) -> v) xxxvs
        as' = reorderArms (sType syms) as
-}
-- For now, just sequential bindings; each recursive
dsExpr syms (ELet [] e) = dsExpr syms e
dsExpr syms (ELet (d:ds) e) =
  let ds' = dsBind syms' d
      syms' = extVals syms (map fst ds')
      e' = dsExpr syms' (ELet ds e)
      def (i, r) a = App (Lam i a) (App (Prim "Y") (Lam i r))
  in  foldr def e' ds'
dsExpr syms (EList es) =
  foldr (app2 cCons) cNil $ map (dsExpr syms) es
dsExpr _ (ETuple []) = Lam "_x" (Var "_x")    -- encoding of ()
dsExpr syms (ETuple [e]) = dsExpr syms e
dsExpr syms (ETuple es) = Lam "_f" $ foldl App (Var "_f") $ map (dsExpr syms) es
dsExpr syms (EStr cs) = dsExpr syms $ EList $ map EChar cs
dsExpr _ (EDo _ []) = error "empty do"
dsExpr _ (EDo _ [SBind _ _]) = error "do without final expression"
dsExpr syms (EDo _ [SThen e]) = dsExpr syms e
dsExpr syms ee@(EDo mn (SBind p e : ss)) =
  let
    nv = newVar (allVarsExpr ee)
    body = ECase (EVar nv) [(p, EDo mn ss), (PVar dummyIdent, eError "dopat")]
  in  dsExpr syms $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam [nv] body)
dsExpr syms (EDo mn (SThen   e : ss)) =
  dsExpr syms $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn ss)
dsExpr syms (EDo mn (SLet ds : ss)) =
  dsExpr syms $ ELet ds (EDo mn ss)
dsExpr _ (EPrim s) = Prim s
dsExpr syms (ESectL e op) =
  App (dsExpr syms (EVar op)) (dsExpr syms e)
dsExpr syms (ESectR op e) =
  app2 cFlip (dsExpr syms (EVar op)) (dsExpr syms e)
dsExpr syms (EIf e1 e2 e3) =
  app2 (dsExpr syms e1) (dsExpr syms e3) (dsExpr syms e2)
dsExpr syms (ECompr e []) = dsExpr syms (EList [e])
dsExpr syms ee@(ECompr e (SBind p b : ss)) =
  let
    nv = newVar (allVarsExpr ee)
    body = ECase (EVar nv) [(p, ECompr e ss), (PVar dummyIdent, EList [])]
  in app2 (Var "Data.List.concatMap") (dsExpr syms (ELam [nv] body)) (dsExpr syms b)
dsExpr syms (ECompr e (SThen c : ss)) =
  dsExpr syms (EIf c (ECompr e ss) (EList []))
dsExpr syms (ECompr e (SLet d : ss)) =
  dsExpr syms (ELet d (ECompr e ss))
dsExpr _ EBad = error "complex case not implemented"

mqual :: Maybe Ident -> Ident -> Ident
mqual (Just qi) i = qual qi i
mqual Nothing   i = i

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
    [] -> ([], ELam [dummyIdent] $ eError "no match")
    arm : arms ->
      case arm of
        (pat, rhs) ->
          case pat of
            PVar i -> ([], ELam [i] rhs)
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

    dsArm (PVar _, _) = undefined -- impossible
    dsArm (PConstr _ ps, rhs) =
      let
        vs = take (length ps) (tail nvs)
        pat (v, p) r = ECase (EVar v) [(p, r), (PVar dummyIdent, EBad)]
        cr = foldr pat rhs (zip vs ps)
      in lams vs $ dsExpr (extVals syms (ev:vs)) cr

  in
    elet $ apps (Var ev) (map dsArm rarms)

asVar :: Ident -> Exp -> (Ident, Exp -> Exp)
asVar i ae =
  case ae of
    Var ii -> (ii, id)
    _ -> (i, \ e -> App (Lam i e) ae)

reorderArms :: [(Ident, Int)] -> [ECaseArm] -> Expr -> [ECaseArm]
reorderArms constrs as ed =
  let arms = [(c, a) | a@(PConstr c _, _) <- as]
      arm (c, k) =
        case lookup c arms of
          Nothing -> (PConstr c (replicate k (PVar dummyIdent)), ed)
          Just a@(PConstr _ ps, _) ->
            if length ps == k then a else error $ "bad contructor arity: " ++ show a
          Just _ -> undefined -- impossible
  in  map arm constrs

constrType :: Ident -> TypeTable -> TypeDef
constrType con tys =
  if take 1 con == "," then
    TypeDef con [(con, untupleConstr con)]
  else
    case M.lookup con tys of
      Nothing -> error $ "undefined constructor: " ++ show con
      Just [td] -> td
      Just _tds -> error $ "ambiguous constructor: " ++ show con

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

dsData :: EDef -> [TypeDef]
dsData (Data (tn, _) cs) = [TypeDef tn [(c, length ts) | (c, ts) <- cs ]]
dsData _ = []

patVars :: EPat -> [Ident]
patVars (PVar i) = [i]
patVars (PConstr _ ps) = concatMap patVars ps

newVars :: [Ident] -> [Ident]
newVars is = [ "nv" ++ show i | i <- [1..] ] \\ is

newVar :: [Ident] -> Ident
newVar = head . newVars

allVarsBind :: EBind -> [Ident]
allVarsBind (BFcn l e) = allVarsLHS l ++ allVarsExpr e
allVarsBind (BPat p e) = allVarsPat p ++ allVarsExpr e

allVarsLHS :: LHS -> [Ident]
allVarsLHS (i, is) = i : is

allVarsPat :: EPat -> [Ident]
allVarsPat (PVar i) = [i]
allVarsPat (PConstr i ps) = i : concatMap allVarsPat ps

allVarsExpr :: Expr -> [Ident]
allVarsExpr (EVar i) = [i]
allVarsExpr (EApp e1 e2) = allVarsExpr e1 ++ allVarsExpr e2
allVarsExpr (ELam is e) = is ++ allVarsExpr e
allVarsExpr (EInt _) = []
allVarsExpr (EChar _) = []
allVarsExpr (EStr _) = []
allVarsExpr (ECase e as) = allVarsExpr e ++ concatMap (\ (p, a) -> allVarsPat p ++ allVarsExpr a) as
allVarsExpr (ELet bs e) = concatMap allVarsBind bs ++ allVarsExpr e
allVarsExpr (ETuple es) = concatMap allVarsExpr es
allVarsExpr (EList es) = concatMap allVarsExpr es
allVarsExpr (EDo mi ss) = maybe [] (:[]) mi ++ concatMap allVarsStmt ss
allVarsExpr (EPrim _) = []
allVarsExpr (ESectL e i) = i : allVarsExpr e
allVarsExpr (ESectR i e) = i : allVarsExpr e
allVarsExpr (EIf e1 e2 e3) = allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3
allVarsExpr (ECompr e ss) = allVarsExpr e ++ concatMap allVarsStmt ss
allVarsExpr EBad = []

allVarsStmt :: EStmt -> [Ident]
allVarsStmt (SBind p e) = allVarsPat p ++ allVarsExpr e
allVarsStmt (SThen e) = allVarsExpr e
allVarsStmt (SLet bs) = concatMap allVarsBind bs
