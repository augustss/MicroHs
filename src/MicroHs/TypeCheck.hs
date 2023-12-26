-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
module MicroHs.TypeCheck(
  typeCheck,
  TModule(..), showTModule, tModuleName,
  impossible, impossibleShow,
  mkClassConstructor,
  mkSuperSel,
  bindingsOf,
  boolPrefix,
  listPrefix,
  ) where
import Prelude
import Control.Alternative
import Control.Arrow(first)
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import MicroHs.TCMonad as T
import qualified MicroHs.IdentMap as M
import MicroHs.Ident
import MicroHs.Deriving
import MicroHs.Expr
import MicroHs.Fixity
import Compat
import GHC.Stack
import Debug.Trace

boolPrefix :: String
boolPrefix = "Data.Bool_Type."

listPrefix :: String
listPrefix = "Data.List_Type."

nameInt :: String
nameInt = "Primitives.Int"

nameWord :: String
nameWord = "Primitives.Word"

nameDouble :: String
nameDouble = "Primitives.Double"

nameChar :: String
nameChar = "Primitives.Char"

nameInteger :: String
nameInteger = "Data.Integer_Type.Integer"

nameTypeEq :: String
nameTypeEq = "Primitives.~"

nameImplies :: String
nameImplies = "Primitives.=>"

nameArrow :: String
nameArrow = "Primitives.->"

nameSymbol :: String
nameSymbol = "Primitives.Symbol"

nameNat :: String
nameNat = "Primitives.Nat"

nameType :: String
nameType = "Primitives.Type"

nameKind :: String
nameKind = "Primitives.Kind"

nameConstraint :: String
nameConstraint = "Primitives.Constraint"

nameKnownNat :: String
nameKnownNat = "Data.TypeLits.KnownNat"

nameKnownSymbol :: String
nameKnownSymbol = "Data.TypeLits.KnownSymbol"

--primitiveKinds :: [String]
--primitiveKinds = [nameType, nameConstraint, nameSymbol, nameNat]

----------------------

data TModule a = TModule
  IdentModule     -- module names
  [FixDef]        -- all fixities, exported or not
  [TypeExport]    -- exported types
  [SynDef]        -- all type synonyms, exported or not
  [ClsDef]        -- all classes
  [InstDef]       -- all instances
  [ValueExport]   -- exported values (including from T(..))
  a               -- bindings
  deriving (Show)

tModuleName :: forall a . TModule a -> IdentModule
tModuleName (TModule a _ _ _ _ _ _ _) = a

bindingsOf :: forall a . TModule a -> a
bindingsOf (TModule _ _ _ _ _ _ _ a) = a

data TypeExport = TypeExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  [ValueExport]   -- associated values, i.e., constructors, selectors, methods
  deriving (Show)

data ValueExport = ValueExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  deriving (Show)

type FixDef = (Ident, Fixity)
type SynDef = (Ident, EType)
type ClsDef = (Ident, ClassInfo)
type InstDef= (Ident, InstInfo)

type Sigma = EType
--type Tau   = EType
type Rho   = EType
type TyVar = Ident

typeCheck :: forall a . [(ImportSpec, TModule a)] -> EModule -> TModule [EDef]
typeCheck aimps (EModule mn exps defs) =
--  trace (unlines $ map (showTModuleExps . snd) aimps) $
  let
    imps = map filterImports aimps
    (fs, ts, ss, cs, is, vs, as) = mkTables imps
  in case tcRun (tcDefs defs) (initTC mn fs ts ss cs is vs as) of
       (tds, tcs) ->
         let
           thisMdl = (mn, mkTModule tds tcs)
           impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ m mm _, tm) <- imps]
           impMap = M.fromList [(i, m) | (i, m) <- thisMdl : impMdls]
           (texps, cexps, vexps) =
             unzip3 $ map (getTVExps impMap (typeTable tcs) (valueTable tcs) (assocTable tcs) (classTable tcs)) exps
           fexps = [ fe | TModule _ fe _ _ _ _ _ _ <- M.elems impMap ]
           sexps = M.toList (synTable tcs)
           iexps = M.toList (instTable tcs)
         in  tModule mn (nubBy ((==) `on` fst) (concat fexps)) (concat texps) sexps (concat cexps) iexps (concat vexps) tds

-- A hack to force evaluation of errors.
-- This should be redone to all happen in the T monad.
tModule :: IdentModule -> [FixDef] -> [TypeExport] -> [SynDef] -> [ClsDef] -> [InstDef] -> [ValueExport] -> [EDef] ->
           TModule [EDef]
tModule mn fs ts ss cs is vs ds =
--  trace ("tmodule " ++ showIdent mn ++ ":\n" ++ show vs) $
  tseq ts `seq` vseq vs `seq` TModule mn fs ts ss cs is vs ds
  where
    tseq [] = ()
    tseq (TypeExport _ e _:xs) = e `seq` tseq xs
    vseq [] = ()
    vseq (ValueExport _ e:xs) = e `seq` vseq xs

filterImports :: forall a . (ImportSpec, TModule a) -> (ImportSpec, TModule a)
filterImports it@(ImportSpec _ _ _ Nothing, _) = it
filterImports (imp@(ImportSpec _ _ _ (Just (hide, is))), TModule mn fx ts ss cs ins vs a) =
  let
    keep x xs = elem x xs /= hide
    ivs = [ i | ImpValue i <- is ]
    vs' = filter (\ (ValueExport i _) -> keep i ivs) vs
    cts = [ i | ImpTypeCon i <- is ]
    its = [ i | ImpType i <- is ] ++ cts
    ts' = map (\ te@(TypeExport i e _) -> if keep i cts then te else TypeExport i e []) $
          filter (\ (TypeExport i _ _) -> keep i its) ts
  in
    --trace (show (ts, vs)) $
    (imp, TModule mn fx ts' ss cs ins vs' a)

-- Type and value exports
getTVExps :: forall a . M.Map (TModule a) -> TypeTable -> ValueTable -> AssocTable -> ClassTable -> ExportItem ->
           ([TypeExport], [ClsDef], [ValueExport])
getTVExps impMap _ _ _ _ (ExpModule m) =
  case M.lookup m impMap of
    Just (TModule _ _ te _ ce _ ve _) -> (te, ce, ve)
    _ -> errorMessage (getSLoc m) $ "undefined module: " ++ showIdent m
getTVExps _ tys vals ast cls (ExpTypeCon i) =
  let
    e = expLookup i tys
    qi = tyQIdent e
    ves = getAssocs vals ast qi
    cl = case M.lookup qi cls of
           Just ci -> [(qi, ci)]
           Nothing -> []
  in ([TypeExport i e ves], cl, [])
getTVExps _ tys _ _ cls (ExpType i) =
  let
    e = expLookup i tys
    qi = tyQIdent e
    cl = case M.lookup qi cls of
           Just ci -> [(qi, ci)]
           Nothing -> []
  in ([TypeExport i e []], cl, [])
getTVExps _ _ vals _ _ (ExpValue i) =
    ([], [], [ValueExport i (expLookup i vals)])

-- Export all fixities and synonyms.
-- The synonyms might be needed, and the fixities are harmless
--getFSExps :: forall a . M.Map (TModule a) -> [([FixDef], [SynDef])]
--getFSExps impMap = [ (fe, se) | TModule _ fe _ se _ _ <- M.elems impMap ]

expLookup :: Ident -> SymTab Entry -> Entry
expLookup i m = either (errorMessage (getSLoc i)) id $ stLookup "export" i m

tyQIdent :: Entry -> Ident
tyQIdent (Entry (EVar qi) _) = qi
tyQIdent _ = error "tyQIdent"

eVarI :: SLoc -> String -> Expr
eVarI loc = EVar . mkIdentSLoc loc

getApp :: HasCallStack => EType -> (Ident, [EType])
getApp = loop []
  where loop as (EVar i) = (i, as)
        loop as (EApp f a) = loop (a:as) f
        loop _ t = impossibleShow t

-- Construct a dummy TModule for the currently compiled module.
-- It has all the relevant export tables.
-- The value&type export tables will later be filtered through the export list.
mkTModule :: forall a . [EDef] -> TCState -> TModule a
mkTModule tds tcs =
  let
    mn = moduleName tcs
    tt = typeTable  tcs
    at = assocTable tcs
    vt = valueTable tcs
    ct = classTable tcs
    it = instTable  tcs

    -- Find the Entry for a type.
    tentry i =
      case stLookup "" (qualIdent mn i) tt of
        Right e -> e
        _       -> impossible
          -- error $ show (qualIdent mn i, M.toList tt)
          
    -- Find all value Entry for names associated with a type.
    assoc i = getAssocs vt at (qualIdent mn i)

    -- All top level values possible to export.
    ves = [ ValueExport i (Entry (EVar (qualIdent mn i)) ts) | Sign i ts <- tds ]

    -- All top level types possible to export.
    tes =
      [ TypeExport i (tentry i) (assoc i) | Data    (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Newtype (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Class _ (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) []        | Type    (i, _) _   <- tds ]

    -- All type synonym definitions.
    ses = [ (qualIdent mn i, EForall vs t) | Type (i, vs) t  <- tds ]

    -- All fixity declaration.
    fes = [ (qualIdent mn i, fx) | Infix fx is <- tds, i <- is ]

    -- All classes
    -- XXX only export the locally defined classes
    ces = M.toList ct

    -- All instances
    ies = M.toList it
  in  TModule mn fes tes ses ces ies ves impossible

-- Find all value Entry for names associated with a type.
getAssocs :: ValueTable -> AssocTable -> Ident -> [ValueExport]
getAssocs vt at ai =
  let qis = fromMaybe [] $ M.lookup ai at
      val qi = case stLookup "" qi vt of
                 Right e -> e
                 _       -> impossible
  in  map (\ qi -> ValueExport (unQualIdent qi) (val qi)) qis

mkTables :: forall a . [(ImportSpec, TModule a)] ->
            (FixTable, TypeTable, SynTable, ClassTable, InstTable, ValueTable, AssocTable)
mkTables mdls =
  let
    qns (ImportSpec q _ mas _) mn i =
      let
        m = fromMaybe mn mas
      in  if q then [qualIdent m i] else [i, qualIdent m i]
    allValues :: ValueTable
    allValues =
      let
        syms (is, TModule mn _ tes _ cls _ ves _) =
          [ (v, [e]) | ValueExport i e    <- ves,                        v <- qns is mn i ] ++
          [ (v, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, v <- qns is mn i ] ++
          [ (v, [Entry (EVar v) t]) | (i, (_, _, t, _, _)) <- cls, let { v = mkClassConstructor i } ]
      in  stFromListWith union $ concatMap syms mdls
    allSyns =
      let
        syns (_, TModule _ _ _ ses _ _ _ _) = ses
      in  M.fromList (concatMap syns mdls)
    allTypes :: TypeTable
    allTypes =
      let
        types (is, TModule mn _ tes _ _ _ _ _) = [ (v, [e]) | TypeExport i e _ <- tes, v <- qns is mn i ]
      in stFromListWith union $ concatMap types mdls
    allFixes =
      let
        fixes (_, TModule _ fes _ _ _ _ _ _) = fes
      in M.fromList (concatMap fixes mdls)
    allAssocs :: AssocTable
    allAssocs =
      let
        assocs (ImportSpec _ _ mas _, TModule mn _ tes _ _ _ _ _) =
          let
            m = fromMaybe mn mas
          in  [ (qualIdent m i, [qualIdent m a | ValueExport a _ <- cs]) | TypeExport i _ cs <- tes ]
      in  M.fromList $ concatMap assocs mdls
    allClasses :: ClassTable
    allClasses =
      let
        clss (_, TModule _ _ _ _ ces _ _ _) = ces
      in  --(\ m -> trace ("allClasses: " ++ showListS showIdentClassInfo (M.toList m)) m) $
          M.fromList $ concatMap clss mdls
    allInsts :: InstTable
    allInsts =
      let
        insts (_, TModule _ _ _ _ _ ies _ _) = ies
      in  M.fromListWith mergeInstInfo $ concatMap insts mdls
  in  (allFixes, allTypes, allSyns, allClasses, allInsts, allValues, allAssocs)

mergeInstInfo :: InstInfo -> InstInfo -> InstInfo
mergeInstInfo (InstInfo m1 l1 fds) (InstInfo m2 l2 _) =
  let
    m = foldr (uncurry $ M.insertWith mrg) m2 (M.toList m1)
    mrg e1 _e2 = e1 -- XXX improve this if eqExpr e1 e2 then e1 else errorMessage (getSLoc e1) $ "Multiple instances: " ++ showSLoc (getSLoc e2)
    l = unionBy eqInstDict l1 l2
  in  InstInfo m l fds

-- Approximate equality for dictionaries.
-- The important thing is to avoid exact duplicates in the instance table.
eqInstDict :: InstDict -> InstDict -> Bool
eqInstDict (e, _, _) (e', _, _) = eqExpr e e'

--------------------------

-- Use the type table as the value table, and the primKind table as the type table.
withTypeTable :: forall a . T a -> T a
withTypeTable ta = do
  otcm <- gets tcMode
  vt <- gets valueTable
  tt <- gets typeTable
  putValueTable tt            -- use type table as value table
  let
    tcm = succ otcm
    next = case tcm of { TCType -> primKindTable; TCKind -> primSortTable; _ -> undefined }
  putTypeTable next           -- use kind/sort table as type table
  putTCMode tcm
  a <- ta
  tt' <- gets valueTable
  putValueTable vt
  putTypeTable tt'
  putTCMode otcm
  return a
  
addAssocTable :: Ident -> [Ident] -> T ()
addAssocTable i ids = modify $ \ ts -> ts { assocTable = M.insert i ids ts.assocTable }

addClassTable :: Ident -> ClassInfo -> T ()
addClassTable i x = modify $ \ ts -> ts { classTable = M.insert i x ts.classTable }

addInstTable :: [InstDictC] -> T ()
addInstTable ics = do
  let
    -- Change type variable to unique unification variables.
    -- These unification variables will never leak, but as an extra caution
    -- we use negative numbers..
    freshSubst iks =
      zipWith (\ ik j -> (idKindIdent ik, EUVar j)) iks [-1, -2 ..]

    mkInstInfo :: InstDictC -> T (Ident, InstInfo)
    mkInstInfo (e, iks, ctx, ct, fds) = do
      case (iks, ctx, getApp ct) of
        ([], [], (c, [EVar i])) -> return $ (c, InstInfo (M.singleton i e) [] fds)
        (_,  _,  (c, ts      )) -> return $ (c, InstInfo M.empty [(e, ctx', ts')] fds)
          where ctx' = map (subst s) ctx
                ts'  = map (subst s) ts
                s    = freshSubst iks
  iis <- mapM mkInstInfo ics
  it <- gets instTable
  putInstTable $ foldr (uncurry $ M.insertWith mergeInstInfo) it iis

addConstraint :: Ident -> EConstraint -> T ()
addConstraint d ctx = do
--  traceM $ "addConstraint: " ++ showIdent d ++ " :: " ++ showEType ctx
  ctx' <- expandSyn ctx
  modify $ \ ts -> ts{ constraints = (d, ctx') : ts.constraints }

withDicts :: forall a . HasCallStack => [(Ident, EConstraint)] -> T a -> T a
withDicts ds ta = do
  ct <- gets ctxTables
  mapM_ addDict ds
  a <- ta
  putCtxTables ct
  return a

withDict :: forall a . HasCallStack => Ident -> EConstraint -> T a -> T a
withDict i c ta = do
  ct <- gets ctxTables
  addDict (i, c)
  a <- ta
  putCtxTables ct
  return a

addDict :: (Ident, EConstraint) -> T ()
addDict (i, c) = do
  c' <- expandSyn c >>= derefUVar
  if null (metaTvs [c']) then
    case c' of
      (EApp (EApp (EVar eq) t1) t2) | eq == mkIdent nameTypeEq -> addEqDict i t1 t2
      _ -> addInstDict i c'
   else
    -- With constraint variables we might get unification variables.
    -- We stash them away in how that we will learn more later.
    addMetaDict i c'

addInstDict :: HasCallStack => Ident -> EConstraint -> T ()
addInstDict i c = do
  c' <- expandSyn c
  ics <- expandDict (EVar i) c'
  addInstTable ics

addEqDict :: Ident -> EType -> EType -> T ()
addEqDict _i t1 t2 = do
  is <- gets typeEqTable
--  traceM ("withEqDict: " ++ show (is, (t1,t2), (addTypeEq t1 t2 is)))
  putTypeEqTable (addTypeEq t1 t2 is)

addMetaDict :: HasCallStack => Ident -> EConstraint -> T ()
addMetaDict i c = do
  ms <- gets metaTable
  putMetaTable ((i,c) : ms)

initTC :: IdentModule -> FixTable -> TypeTable -> SynTable -> ClassTable -> InstTable -> ValueTable -> AssocTable -> TCState
initTC mn fs ts ss cs is vs as =
--  trace ("**** initTC " ++ showIdent mn ++ ": " ++ showListS (showPairS showIdent showEType) (M.toList ss)) $
  let
    xts = foldr (uncurry stInsertGlb) ts primTypes
    xvs = foldr (uncurry stInsertGlb) vs primValues
  in TC { moduleName = mn, unique = 1, fixTable = addPrimFixs fs, typeTable = xts,
          synTable = ss, valueTable = xvs, assocTable = as, uvarSubst = IM.empty,
          tcMode = TCExpr, classTable = cs, ctxTables = (is,[],[]), constraints = [], defaults = [] }

addPrimFixs :: FixTable -> FixTable
addPrimFixs =
  M.insert (mkIdent "Primitives.->") (AssocRight, -1) .
  M.insert (mkIdent "Primitives.=>") (AssocRight, -2)

-- r for 'realm', suggested by ChatGPT
rSort :: ESort
rSort = EVar (Ident noSLoc "Primitives.Sort")

sKindKindKind :: EKind
sKindKindKind = sArrow sKind (sArrow sKind sKind)

kTypeTypeS :: EType
kTypeTypeS = kArrow kType kType

kTypeTypeTypeS :: EType
kTypeTypeTypeS = kArrow kType $ kArrow kType kType

-- (=>) :: Constraint -> Type -> Type
--kConstraintTypeTypeS :: EType
--kConstraintTypeTypeS = kArrow kConstraint $ kArrow kType kType

-- (~) :: Type -> Type -> Constraint
kTypeTypeConstraintS :: EType
kTypeTypeConstraintS = kArrow kType (kArrow kType kConstraint)

builtinLoc :: SLoc
builtinLoc = SLoc "builtin" 0 0

mkIdentB :: String -> Ident
mkIdentB = mkIdentSLoc builtinLoc

-- E.g.
--  Kind :: Sort
primSortTable :: KindTable
primSortTable =
  let
    entry i = Entry (EVar (mkIdentB i))
  in stFromList [
       -- The kinds are wired in (for now)
       (mkIdentB nameKind,       [entry nameKind rSort]),
       (mkIdentB "Kind",         [entry nameKind rSort])
     ]

-- E.g.
--  Type       :: Kind
--  Constraint :: Kind
--  (->)       :: Kind -> Kind -> Kind
primKindTable :: KindTable
primKindTable =
  let
    entry i = Entry (EVar (mkIdentB i))
  in stFromList [
       -- The kinds are wired in (for now)
       (mkIdentB nameType,       [entry nameType sKind]),
       (mkIdentB "Type",         [entry nameType sKind]),
       (mkIdentB nameConstraint, [entry nameConstraint sKind]),
       (mkIdentB "Constraint",   [entry nameConstraint sKind]),
       (mkIdentB nameSymbol,     [entry nameSymbol sKind]),
       (mkIdentB "Symbol",       [entry nameSymbol sKind]),
       (mkIdentB nameNat,        [entry nameNat sKind]),
       (mkIdentB "Nat",          [entry nameNat sKind]),
       (mkIdentB nameArrow,      [entry nameArrow sKindKindKind]),
       (mkIdentB "->",           [entry nameArrow sKindKindKind])
       ]

-- E.g.
--  Bool  :: Type
--  Int   :: Type
--  (->)  :: Type -> Type -> Type
--  (=>)  :: forall k . Constraint -> k -> k
--  Maybe :: Type -> Type
primTypes :: [(Ident, [Entry])]
primTypes =
  let
    entry i = Entry (EVar (mkIdentB i))
    k = mkIdent "k"
    kv = EVar k
    kk = IdKind k sKind
    -- Tuples are polykinded since they need to handle both Type and Constraint
    tuple n =
      let
        i = tupleConstr builtinLoc n
      in  (i, [entry (unIdent i) $ EForall [kk] $ foldr kArrow kv (replicate n kv)])
    kImplies = EForall [kk] $ kConstraint `kArrow` (kv `kArrow` kv)
  in
      [
       -- The function arrow et al are bothersome to define in Primitives, so keep them here.
       -- But the fixity is defined in Primitives.
       (mkIdentB "->",           [entry nameArrow    kTypeTypeTypeS]),
       (mkIdentB "=>",           [entry nameImplies  kImplies]),
       (mkIdentB "~",            [entry nameTypeEq   kTypeTypeConstraintS]),
       -- Primitives.hs uses the type [], and it's annoying to fix that.
       -- XXX should not be needed
       (mkIdentB (listPrefix ++ "[]"), [entry (listPrefix ++ "[]")        kTypeTypeS])
      ] ++
      map tuple (enumFromTo 2 10)

-- E.g.
--  True :: Bool
--  (&&) :: Bool -> Bool
--  Just :: forall a . a -> Maybe a
--  ,    :: forall a b . a -> b -> (a,b)
primValues :: [(Ident, [Entry])]
primValues =
  let
    tuple n =
      let
        c = tupleConstr builtinLoc n
        vks = [IdKind (mkIdent ("a" ++ show i)) kType | i <- enumFromTo 1 n]
        ts = map tVarK vks
        r = tApps c ts
      in  (c, [Entry (ECon $ ConData [(c, n)] c []) $ EForall vks $ foldr tArrow r ts ])
  in  map tuple (enumFromTo 2 10)

tCon :: Ident -> EType
tCon = EVar

tVarK :: IdKind -> EType
tVarK (IdKind i _) = EVar i

tApp :: EType -> EType -> EType
tApp = EApp

tApps :: Ident -> [EType] -> EType
tApps i ts = foldl tApp (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = tApp (tApp (tConI builtinLoc "Primitives.->") a) r

tImplies :: EType -> EType -> EType
tImplies a r = tApp (tApp (tConI builtinLoc "Primitives.=>") a) r

kArrow :: EKind -> EKind -> EKind
kArrow = tArrow

sArrow :: ESort -> ESort -> ESort
sArrow = tArrow

{-
isArrow :: EType -> Bool
isArrow = isJust . getArrow
-}

getArrow :: EType -> Maybe (EType, EType)
getArrow (EApp (EApp (EVar n) a) b) =
  if isIdent "->" n || isIdent "Primitives.->" n then Just (a, b) else Nothing
getArrow _ = Nothing

getImplies :: EType -> Maybe (EType, EType)
getImplies (EApp (EApp (EVar n) a) b) =
  if isIdent "=>" n || isIdent "Primitives.=>" n then Just (a, b) else Nothing
getImplies _ = Nothing

{-
getTuple :: Int -> EType -> Maybe [EType]
getTuple n t = loop t []
  where loop (EVar i) r | isTupleConstr n i && length r == n = Just (reverse r)
        loop (EApp f a) r = loop f (a:r)
        loop _ _ = Nothing
-}

setUVar :: TRef -> EType -> T ()
setUVar i t = modify $ \ ts -> ts{ uvarSubst = IM.insert i t ts.uvarSubst }

getUVar :: Int -> T (Maybe EType)
getUVar i = gets (IM.lookup i . uvarSubst)

munify :: HasCallStack =>
          SLoc -> Expected -> EType -> T ()
munify loc (Infer r) b = tSetRefType loc r b
munify loc (Check a) b = unify loc a b

expandSyn :: HasCallStack =>
             EType -> T EType
expandSyn at =
  let
    syn ts t =
      case t of
        EApp f a -> do
          aa <- expandSyn a
          syn (aa:ts) f
        EVar i -> do
          syns <- gets synTable
          case M.lookup i syns of
            Nothing -> return $ foldl tApp t ts
            Just (EForall vks tt) ->
--              if length vks /= length ts then tcError (getSLoc i) $ "bad synonym use"
--              else expandSyn $ subst (zip (map idKindIdent vks) ts) tt
              let s = zip (map idKindIdent vks) ts
                  lvks = length vks
                  lts = length ts
              in  case compare lvks lts of
                    LT -> expandSyn $ foldl EApp (subst s tt) (drop lvks ts)
                    EQ -> expandSyn $ subst s tt
                    GT -> tcError (getSLoc i) $ "bad synonym use"
                          --EForall (drop lts vks) (subst s tt)
            Just _ -> impossible
        EUVar _ -> return $ foldl tApp t ts
        ESign a _ -> expandSyn a   -- Throw away signatures, they don't affect unification
        EForall iks tt | null ts -> EForall iks <$> expandSyn tt
        ELit _ (LStr _) -> return t
        ELit _ (LInteger _) -> return t
        _ -> impossible
  in syn [] at

mapEType :: (EType -> EType) -> EType -> EType
mapEType fn = rec
  where
    rec (EApp f a) = EApp (rec f) (rec a)
    rec (ESign t k) = ESign (rec t) k
    rec (EForall iks t) = EForall iks (rec t)
    rec t = fn t

derefUVar :: EType -> T EType
derefUVar at =
  case at of
    EApp f a -> do
      fx <- derefUVar f
      ax <- derefUVar a
      return $ EApp fx ax
    EUVar i -> do
      mt <- getUVar i
      case mt of
        Nothing -> return at
        Just t -> do
          t' <- derefUVar t
          setUVar i t'
          return t'
    EVar _ -> return at
    ESign t k -> flip ESign k <$> derefUVar t
    EForall iks t -> EForall iks <$> derefUVar t
    ELit _ (LStr _) -> return at
    ELit _ (LInteger _) -> return at
    _ -> impossible

tcErrorTK :: HasCallStack =>
             SLoc -> String -> T ()
tcErrorTK loc msg = do
  tcm <- gets tcMode
  tcError loc $ msgTCMode' tcm ++ " error: " ++ msg

-- For error messages
msgTCMode :: TCMode -> String
msgTCMode TCExpr = "value"
msgTCMode TCType = "type"
msgTCMode TCKind = "kind"
msgTCMode TCSort = "sort"

msgTCMode' :: TCMode -> String
msgTCMode' TCExpr = "type"
msgTCMode' TCType = "kind"
msgTCMode' TCKind = "sort"
msgTCMode' TCSort = "realm"

unify :: HasCallStack =>
         SLoc -> EType -> EType -> T ()
unify loc a b = do
  aa <- expandSyn a
  bb <- expandSyn b
  unifyR loc aa bb

-- XXX should do occur check
unifyR :: HasCallStack =>
          SLoc -> EType -> EType -> T ()
unifyR _   (EVar x1)    (EVar x2)  | x1 == x2      = return ()
unifyR loc (EApp f1 a1) (EApp f2 a2)               = do { unifyR loc f1 f2; unifyR loc a1 a2 }
unifyR _   (EUVar r1)   (EUVar r2) | r1 == r2      = return ()
unifyR loc (EUVar r1)   t2                         = unifyVar loc r1 t2
unifyR loc t1           (EUVar r2)                 = unifyVar loc r2 t1
unifyR loc t1           t2                         = do
  tcm <- gets tcMode
  case tcm of
    -- Defer to constraint solver.
    -- XXX needs changing if we have kind equalities.
    TCExpr -> addEqConstraint loc t1 t2
    _      -> tcErrorTK loc $ "cannot unify " ++ showExpr t1 ++ " and " ++ showExpr t2

unifyVar :: HasCallStack =>
            SLoc -> TRef -> EType -> T ()
unifyVar loc r t = do
  mt <- getUVar r
  case mt of
    Nothing -> unifyUnboundVar loc r t
    Just t' -> unify loc t' t

unifyUnboundVar :: HasCallStack =>
                   SLoc -> TRef -> EType -> T ()
unifyUnboundVar loc r1 at2@(EUVar r2) = do
  -- We know r1 /= r2
  mt2 <- getUVar r2
  case mt2 of
    Nothing -> setUVar r1 at2
    Just t2 -> unify loc (EUVar r1) t2
unifyUnboundVar loc r1 t2 = do
  vs <- getMetaTyVars [t2]
  if elemBy (==) r1 vs then
    tcErrorTK loc $ "cyclic " ++ showExpr (EUVar r1) ++ " = " ++ showExpr t2
   else
    setUVar r1 t2

-- Reset unification map
tcReset :: T ()
tcReset = modify $ \ ts -> ts{ uvarSubst = IM.empty }

newUVar :: T EType
newUVar = EUVar <$> newUniq

uniqIdentSep :: String
uniqIdentSep = "$"

newIdent :: SLoc -> String -> T Ident
newIdent loc s = do
  u <- newUniq
  return $ mkIdentSLoc loc $ s ++ uniqIdentSep ++ show u

tLookup :: HasCallStack =>
           String -> Ident -> T (Expr, EType)
tLookup msg i = do
  env <- gets valueTable
  case stLookup msg i env of
    Right (Entry e s) -> return (setSLocExpr (getSLoc i) e, s)
    Left            e -> do
{-
      tcm <- gets tcMode
      traceM ("TCMode=" ++ show tcm)
      traceM ("Value table:\n" ++ show env)
      tenv <- gets typeTable
      traceM ("Type table:\n" ++ show tenv)
-}
      tcError (getSLoc i) e

tLookupV :: HasCallStack =>
           Ident -> T (Expr, EType)
tLookupV i = do
  tcm <- gets tcMode
  tLookup (msgTCMode tcm) i

tInst :: HasCallStack => (Expr, EType) -> T (Expr, EType)
tInst (ae, EForall vks t) = tInstForall ae vks t >>= tInst
tInst (ae, at) | Just (ctx, t) <- getImplies at = do
  d <- newDictIdent (getSLoc ae)
  --traceM $ "tInst: addConstraint: " ++ show ae ++ ", " ++ show d ++ " :: " ++ show ctx
  addConstraint d ctx
  tInst (EApp ae (EVar d), t)
tInst at = return at

tInstForall :: Expr -> [IdKind] -> EType -> T (Expr, EType)
tInstForall ae vks t =
  if null vks then
    return (ae, t)
  else do
    let vs = map idKindIdent vks
    us <- mapM (const newUVar) vks
    return (ae, subst (zip vs us) t)

tInst' :: (Expr, EType) -> T (Expr, EType)
tInst' (ae, EForall vks t) = tInstForall ae vks t
tInst' et = return et

extValE :: HasCallStack =>
           Ident -> EType -> Expr -> T ()
extValE i t e = do
  venv <- gets valueTable
  putValueTable (stInsertLcl i (Entry e t) venv)

-- Extend the global symbol table with i = e :: t
-- Add both qualified and unqualified versions of i.
extValETop :: HasCallStack =>
              Ident -> EType -> Expr -> T ()
extValETop i t e = do
  mn <- gets moduleName
  venv <- gets valueTable
  let qi = qualIdent mn i
      venv'  = stInsertGlb qi [Entry e t] venv
      venv'' = stInsertGlb  i [Entry e t] venv'
  putValueTable venv''

-- Extend symbol table with i::t.
-- The translation for i will be the qualified name.
-- Add both qualified and unqualified versions of i.
extValQTop :: HasCallStack =>
              Ident -> EType -> T ()
extValQTop i t = do
  mn <- gets moduleName
  extValETop i t (EVar (qualIdent mn i))

extVal :: HasCallStack =>
          Ident -> EType -> T ()
extVal i t = extValE i t $ EVar i

extVals :: HasCallStack =>
           [(Ident, EType)] -> T ()
extVals = mapM_ (uncurry extVal)

extTyp :: Ident -> EType -> T ()
extTyp i t = do
  tenv <- gets typeTable
  putTypeTable (stInsertLcl i (Entry (EVar i) t) tenv)

extTyps :: [(Ident, EType)] -> T ()
extTyps = mapM_ (uncurry extTyp)

extSyn :: Ident -> EType -> T ()
extSyn i t = do
  senv <- gets synTable
  putSynTable (M.insert i t senv)

extFix :: Ident -> Fixity -> T ()
extFix i fx = modify $ \ ts -> ts{ fixTable = M.insert i fx ts.fixTable }

withExtVal :: forall a . HasCallStack =>
              Ident -> EType -> T a -> T a
withExtVal i t ta = do
  venv <- gets valueTable
  extVal i t
  a <- ta
  putValueTable venv
  return a

withExtVals :: forall a . HasCallStack =>
               [(Ident, EType)] -> T a -> T a
withExtVals env ta = do
  venv <- gets valueTable
  extVals env
  a <- ta
  putValueTable venv
  return a

withExtTyps :: forall a . [IdKind] -> T a -> T a
withExtTyps iks ta = do
  let env = map (\ (IdKind v k) -> (v, k)) iks
  venv <- gets typeTable
  extTyps env
  a <- ta
  putTypeTable venv
  return a

tcDefs :: [EDef] -> T [EDef]
tcDefs ds = do
--  traceM ("tcDefs 1:\n" ++ showEDefs ds)
  mapM_ tcAddInfix ds
  dst <- tcDefsType ds
--  traceM ("tcDefs 2:\n" ++ showEDefs dst)
  mapM_ addTypeSyn dst
  dst' <- tcExpand dst
--  traceM ("tcDefs 3:\n" ++ showEDefs dst')
  setDefault dst'
  tcDefsValue dst'

setDefault :: [EDef] -> T ()
setDefault defs =
  putDefaults $ last $ [] : [ ts | Default ts <- defs ]

tcAddInfix :: EDef -> T ()
tcAddInfix (Infix fx is) = do
  mn <- gets moduleName
  mapM_ (\ i -> extFix (qualIdent mn i) fx) is
tcAddInfix _ = return ()

-- Check type definitions
tcDefsType :: HasCallStack => [EDef] -> T [EDef]
tcDefsType ds = withTypeTable $ do
  kindSigs <- getKindSigns ds
  mapM_ (addTypeKind kindSigs) ds              -- Add the kind of each type to the environment
  dst <- mapM tcDefType ds                     -- Kind check all top level type expressions
--  vars <- gets uvarSubst
--  traceM $ show vars
  vt <- gets valueTable
  let ent (Entry i t) = Entry i . mapEType def <$> derefUVar t
      def (EUVar _) = kType    -- default kind variables to Type
      def t = t
  vt' <- mapMSymTab ent vt
  putValueTable vt'
--  traceM $ "tcDefType value table:\n" ++ show vt'
  return dst

-- Get all kind signatures, and do sort checking of them.
getKindSigns :: HasCallStack => [EDef] -> T (M.Map EKind)
getKindSigns ds = do
  let iks = [ (i, k) | KindSign i k <- ds ]
      kind (i, k) = (i,) <$> tcKind (Check sKind) k
  multCheck (map fst iks)
  iks' <- mapM kind iks
  return $ M.fromList iks'

-- Expand class and instance definitions (must be done after type synonym processing)
tcExpand :: [EDef] -> T [EDef]
tcExpand dst = withTypeTable $ do
  dsc <- concat <$> mapM expandClass dst       -- Expand all class definitions
  dsf <- concat <$> mapM expandField dsc       -- Add HasField instances
  dsd <- concat <$> mapM doDeriving  dsf       -- Add derived instances
--  traceM $ showEDefs dsd
  dsi <- concat <$> mapM expandInst  dsd       -- Expand all instance definitions
  return dsi

-- Check&rename the given kinds, also insert the type variables in the symbol table.
withVks :: forall a . HasCallStack => [IdKind] -> ([IdKind] -> T a) -> T a
withVks vks fun = assertTCMode (>=TCType) $ do
  tcm <- gets tcMode
  let
    expect = case tcm of { TCType -> sKind; TCKind -> rSort; _ -> undefined }
    loop r [] = fun (reverse r)
    loop r (IdKind i mk : iks) = do
      k' <-
        case mk of
          EVar d | d == dummyIdent -> newUVar
          _                        -> withTypeTable $ tcExpr (Check expect) mk   -- bump to next level
      withExtVal i k' $ loop (IdKind i k' : r) iks
  loop [] vks

-- Add symbol a table entry (with kind) for each top level typeish definition.
-- If there is a kind signature, use it.  If not, use a kind variable.
addTypeKind :: M.Map EKind -> EDef -> T ()
addTypeKind kdefs adef = do
  let
    addAssoc i is = do
      mn <- gets moduleName
      addAssocTable (qualIdent mn i) (map (qualIdent mn) is)
    assocData (Constr _ _ c (Left _)) = [c]
    assocData (Constr _ _ c (Right its)) = c : map fst its
    addDef (i, _) = do
      k <-
        case M.lookup i kdefs of
           Nothing -> newUVar
           Just k' -> return k'
      extValQTop i k
      
  case adef of
    Data    lhs@(i, _) cs _ -> do
      addDef lhs
      addAssoc i (nub $ concatMap assocData cs)
    Newtype lhs@(i, _) c  _ -> do
      addDef lhs
      addAssoc i (assocData c)
    Type    lhs _           ->
      addDef lhs
    Class _ lhs@(i, _) _ ms -> do
      addDef lhs
      addAssoc i [ x | BSign m _ <- ms, x <- [m, mkDefaultMethodId m] ]
    _ -> return ()

-- Add type synonyms to the synonym table
addTypeSyn :: EDef -> T ()
addTypeSyn adef =
  case adef of
    Type (i, vs) t -> do
      let t' = EForall vs t
      extSyn i t'
      mn <- gets moduleName
      extSyn (qualIdent mn i) t'
    _ -> return ()

-- Do kind checking of all typeish definitions.
tcDefType :: HasCallStack => EDef -> T EDef
tcDefType def = do
  --tcReset
  case def of
    Data    lhs cs ds      -> withLHS lhs $ \ lhs' -> (,kType)       <$> (Data    lhs'  <$> mapM tcConstr cs <*> mapM tcDerive ds)
    Newtype lhs c  ds      -> withLHS lhs $ \ lhs' -> (,kType)       <$> (Newtype lhs'  <$> tcConstr c       <*> mapM tcDerive ds)
    Type    lhs t          -> withLHS lhs $ \ lhs' -> first              (Type    lhs') <$> tInferTypeT t
    Class   ctx lhs fds ms -> withLHS lhs $ \ lhs' -> (,kConstraint) <$> (Class         <$> tcCtx ctx <*> return lhs' <*> mapM tcFD fds <*> mapM tcMethod ms)
    Sign      i t          ->                                             Sign      i   <$> tCheckTypeT kType t
    ForImp ie i t          ->                                             ForImp ie i   <$> tCheckTypeT kType t
    Instance ct m          ->                                             Instance      <$> tCheckTypeT kConstraint ct <*> return m
    Default ts             ->                                             Default       <$> mapM (tCheckTypeT kType) ts
    _                      -> return def
 where
   tcMethod (BSign i t) = BSign i <$> tcTypeT (Check kType) t
   tcMethod m = return m
   tcFD (is, os) = (,) <$> mapM tcV is <*> mapM tcV os
     where tcV i = do { _ <- tLookup "fundep" i; return i }
   tcDerive = tCheckTypeT (kType `kArrow` kConstraint)

withLHS :: forall a . HasCallStack => LHS -> (LHS -> T (a, EKind)) -> T a
withLHS (i, vks) ta = do
  (_, ki) <- tLookupV i
  withVks vks $ \ vks' -> do
    (a, kr) <- ta (i, vks')
    let kapp = foldr kArrow kr (map (\ (IdKind _ k) -> k) vks')
    --unify (getSLoc i) ki kapp
    _ <- subsCheckRho (getSLoc i) undefined ki kapp
    return a

tcCtx :: HasCallStack => [EConstraint] -> T [EConstraint]
tcCtx = mapM (tCheckTypeT kConstraint)

tcConstr :: HasCallStack => Constr -> T Constr
tcConstr (Constr iks ct c ets) =
  assertTCMode (==TCType) $
  withVks iks $ \ iks' ->
    Constr iks' <$> tcCtx ct <*> pure c <*>
      either (\ x -> Left  <$> mapM (\ (s,t)     ->         (s,)  <$> tcTypeT (Check kType) t) x)
             (\ x -> Right <$> mapM (\ (i,(s,t)) -> ((i,) . (s,)) <$> tcTypeT (Check kType) t) x) ets

-- Expand a class defintion to
--  * a "data" type for the dictionary, with kind Constraint
--  * superclass selectors
--  * method selectors
--  * default methods
-- E.g.
--   class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> a
--     x /= y = not (x == y)
-- expands to
--   data Eq a = Eq$ (a -> a -> Bool) (a -> a -> Bool)
--               :: Constraint
--   == :: forall a . Eq a -> (a -> a -> Bool)
--   == (Eq x _) = x
--   /= :: forall a . Eq a -> (a -> a -> Bool)
--   /= (Eq _ x) = x
--   ==$dflt :: forall a . (Eq a) => (a -> a -> Bool)
--   ==$dflt = _noDefault "Eq.=="
--   /=$dflt :: forall a . (Eq a) => (a -> a -> Bool)
--   /=$dflt x y = not (x == y)
--
--   class (Eq a) => Ord a where
--     (<=) :: a -> a -> Bool
-- expands to
--   data Ord a = Ord$ (Eq a) (a -> a -> Bool)
--   Ord$super1 :: forall a . Ord a -> Eq a
--   <= :: forall a . Ord a -> (a -> a -> Bool)
--   <=$dflt = _noDefault "Ord.<="
--
--   instance Eq Int where (==) = primEqInt
-- expands to
--   inst$999 = Eq$ meth$1 meth$2
--     where meth$1 = primEqInt
--           meth$2 = /=$dflt dict$999
--
--   instance Ord Int where (<=) = primLEInt
-- expands to
--   inst$888 = Ord$ dict$ meth$1
--     where meth$1 = primLEInt
-- where dict$ is a special magic identifier that the type checker expands
-- to whatever dictionary is forced by the type.
-- In this case (dict$ :: Eq Int), so it with be inst$999
--
-- The actual definitions for the constructor and methods are added
-- in the desugaring pass.
-- Default methods are added as actual definitions.
-- The constructor and methods are added to the symbol table in addValueType.
-- XXX FunDep
expandClass :: EDef -> T [EDef]
expandClass dcls@(Class ctx (iCls, vks) fds ms) = do
  mn <- gets moduleName
  let
      meths = [ b | b@(BSign _ _) <- ms ]
      methIds = map (\ (BSign i _) -> i) meths
      mdflts = [ (i, eqns) | BFcn i eqns <- ms ]
      tCtx = tApps (qualIdent mn iCls) (map (EVar . idKindIdent) vks)
      mkDflt (BSign methId t) = [ Sign iDflt $ EForall vks $ tCtx `tImplies` t, def $ lookup methId mdflts ]
        where def Nothing = Fcn iDflt $ simpleEqn noDflt
              def (Just eqns) = Fcn iDflt eqns
              iDflt = mkDefaultMethodId methId
              -- XXX This isn't right, "Prelude._nodefault" might not be in scope
              noDflt = EApp noDefaultE (ELit noSLoc (LStr (unIdent iCls ++ "." ++ unIdent methId)))
      mkDflt _ = impossible
      dDflts = concatMap mkDflt meths
  addClassTable (qualIdent mn iCls) (vks, ctx, EUVar 0, methIds, mkIFunDeps (map idKindIdent vks) fds)   -- Initial entry, no type needed.
  return $ dcls : dDflts
expandClass d = return [d]

simpleEqn :: Expr -> [Eqn]
simpleEqn e = [Eqn [] $ EAlts [([], e)] []]

-- Keep the list empty if there are no fundeps
mkIFunDeps :: [Ident] -> [FunDep] -> [IFunDep]
--mkIFunDeps vs [] = [(map (const True) vs, map (const False) vs)]
mkIFunDeps vs fds = map (\ (is, os) -> (map (`elem` is) vs, map (`elem` os) vs)) fds

noDefaultE :: Expr
noDefaultE = ELit noSLoc $ LPrim "noDefault"

-- Turn (unqualified) class and method names into a default method name
mkDefaultMethodId :: Ident -> Ident
mkDefaultMethodId meth = addIdentSuffix meth "$dflt"

{-
clsToDict :: EType -> T EType
clsToDict = do
  -- XXX for now, only allow contexts of the form (C t1 ... tn)
  let usup as (EVar c) | isConIdent c = return (tApps c as)
      usup as (EApp f a) = usup (a:as) f
      usup _ t = tcError (getSLoc t) ("bad context " ++ showEType t)
  usup []
-}

addConstraints :: [EConstraint] -> EType -> EType
addConstraints []  t = t
addConstraints cs  t = tupleConstraints cs `tImplies` t

tupleConstraints :: [EConstraint] -> EConstraint
tupleConstraints []  = error "tupleConstraints"
tupleConstraints [c] = c
tupleConstraints cs  = tApps (tupleConstr noSLoc (length cs)) cs

splitInst :: EConstraint -> ([IdKind], [EConstraint], EConstraint)
splitInst (EForall iks t) =
  case splitInst t of
    (iks', ctx, ct) -> (iks ++ iks', ctx, ct)
splitInst act | Just (ctx, ct) <- getImplies act =
  case splitInst ct of
    (iks, ctxs, ct') -> (iks, ctx : ctxs, ct')
splitInst ct = ([], [], ct)

expandInst :: EDef -> T [EDef]
expandInst dinst@(Instance act bs) = do
  (vks, ctx, cc) <- splitInst <$> expandSyn act
  let loc = getSLoc act
      qiCls = getAppCon cc
  iInst <- newIdent loc "inst"
  let sign = Sign iInst act
--  (e, _) <- tLookupV iCls
  ct <- gets classTable
--  let qiCls = getAppCon e
  (_, supers, _, mis, fds) <-
    case M.lookup qiCls ct of
      Nothing -> tcError loc $ "not a class " ++ showIdent qiCls
      Just x -> return x
  -- XXX this ignores type signatures and other bindings
  -- XXX should tack on signatures with ESign
  let ies = [(i, ELam qs) | BFcn i qs <- bs]
      meth i = fromMaybe (ELam $ simpleEqn $ EVar $ setSLocIdent loc $ mkDefaultMethodId i) $ lookup i ies
      meths = map meth mis
      sups = map (const (EVar $ mkIdentSLoc loc dictPrefixDollar)) supers
      args = sups ++ meths
  case map fst ies \\ mis of
    [] -> return ()
    i:_ -> tcError (getSLoc i) $ "superflous binding " ++ show i
  let bind = Fcn iInst $ eEqns [] $ foldl EApp (EVar $ mkClassConstructor qiCls) args
  mn <- gets moduleName
  addInstTable [(EVar $ qualIdent mn iInst, vks, ctx, cc, fds)]
  return [dinst, sign, bind]
expandInst d = return [d]

---------------------

tcDefsValue :: [EDef] -> T [EDef]
tcDefsValue ds = do
  mapM_ addValueType ds
  mapM (\ d -> do { tcReset; tcDefValue d}) ds

addValueType :: EDef -> T ()
addValueType adef = do
  mn <- gets moduleName
  case adef of
    Sign i t -> extValQTop i t
    Data (i, vks) cs _ -> do
      let
        cti = [ (qualIdent mn c, either length length ets + if null ctx then 0 else 1) | Constr _ ctx c ets <- cs ]
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
        addCon (Constr evks ectx c ets) = do
          let ts = either id (map snd) ets
              cty = EForall vks $ EForall evks $ addConstraints ectx $ foldr (tArrow . snd) tret ts
              fs = either (const []) (map fst) ets
          extValETop c cty (ECon $ ConData cti (qualIdent mn c) fs)
      mapM_ addCon cs
    Newtype (i, vks) (Constr _ _ c ets) _ -> do
      let
        t = snd $ head $ either id (map snd) ets
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
        fs = either (const []) (map fst) ets
      extValETop c (EForall vks $ EForall [] $ tArrow t tret) (ECon $ ConNew (qualIdent mn c) fs)
    ForImp _ i t -> extValQTop i t
    Class ctx (i, vks) fds ms -> addValueClass ctx i vks fds ms
    _ -> return ()

-- XXX FunDep
addValueClass :: [EConstraint] -> Ident -> [IdKind] -> [FunDep] -> [EBind] -> T ()
addValueClass ctx iCls vks fds ms = do
  mn <- gets moduleName
  let
      meths = [ b | b@(BSign _ _) <- ms ]
      methTys = map (\ (BSign _ t) -> t) meths
      methIds = map (\ (BSign i _) -> i) meths
      supTys = ctx  -- XXX should do some checking
      targs = supTys ++ methTys
      qiCls = qualIdent mn iCls
      tret = tApps qiCls (map tVarK vks)
      cti = [ (qualIdent mn iCon, length targs) ]
      iCon = mkClassConstructor iCls
      iConTy = EForall vks $ foldr tArrow tret targs
  extValETop iCon iConTy (ECon $ ConData cti (qualIdent mn iCon) [])
  let addMethod (BSign i t) = extValETop i (EForall vks $ tApps qiCls (map (EVar . idKindIdent) vks) `tImplies` t) (EVar $ qualIdent mn i)
      addMethod _ = impossible
--  traceM ("addValueClass " ++ showEType (ETuple ctx))
  mapM_ addMethod meths
  -- Update class table, now with actual constructor type.
  addClassTable qiCls (vks, ctx, iConTy, methIds, mkIFunDeps (map idKindIdent vks) fds)

{-
bundleConstraints :: [EConstraint] -> EType -> EType
bundleConstraints []  t = t
bundleConstraints [c] t = tImplies c t
bundleConstraints cs  t = tImplies (ETuple cs) t
-}

mkClassConstructor :: Ident -> Ident
mkClassConstructor i = addIdentSuffix i "$C"

{-
unForall :: EType -> ([IdKind], EType)
unForall (EForall iks t) = (iks, t)
unForall t = ([], t)
-}

tcDefValue :: HasCallStack =>
              EDef -> T EDef
tcDefValue adef =
  assertTCMode (==TCExpr) $
  case adef of
    Fcn i eqns -> do
      (_, tt) <- tLookup "type signature" i
--      traceM $ "tcDefValue: " ++ showIdent i ++ " :: " ++ showExpr tt
--      traceM $ "tcDefValue:      " ++ showEDefs [adef]
      mn <- gets moduleName
      teqns <- tcEqns True tt eqns
--      traceM ("tcDefValue: after " ++ showEDefs [adef, Fcn i teqns])
      checkConstraints
      return $ Fcn (qualIdent mn i) teqns
    ForImp ie i t -> do
      mn <- gets moduleName
      return (ForImp ie (qualIdent mn i) t)
    _ -> return adef

tCheckTypeT :: HasCallStack => EType -> EType -> T EType
tCheckTypeT = tCheck tcTypeT

tInferTypeT :: HasCallStack => EType -> T (EType, EKind)
tInferTypeT t = tInfer tcTypeT t

-- Kind check a type while already in type checking mode
tcTypeT :: HasCallStack =>
           Expected -> EType -> T EType
tcTypeT mk t = assertTCMode (==TCType) $ tcExpr mk (dsType t)

-- Kind check a type while in value checking mode
tcType :: HasCallStack =>
          Expected -> EType -> T EType
tcType mk = assertTCMode (==TCExpr) . withTypeTable . tcTypeT mk

-- Sort check a kind while already in sort checking mode
tcKindT :: HasCallStack =>
           Expected -> EKind -> T EKind
tcKindT mk t =
--  trace ("tcKindT: " ++ show (mk, t)) $
  assertTCMode (==TCKind) $ tcExpr mk t

-- Sort check a kind while in type checking mode
tcKind :: HasCallStack =>
          Expected -> EKind -> T EKind
tcKind mk = assertTCMode (==TCType) . withTypeTable . tcKindT mk

-- When inferring the type, the resulting type will
-- be assigned to the TRef (using tSetRefType),
-- and can then be read of (using tGetRefType).
-- When checking, the expected type is simple given.
data Expected = Infer TRef | Check EType
--  deriving(Show)

instance Show Expected where
  show (Infer r) = "(Infer " ++ show r ++ ")"
  show (Check t) = "(Check " ++ show t ++ ")"

tInfer :: forall a b . HasCallStack =>
          (Expected -> a -> T b) -> a -> T (Typed b)
tInfer tc a = do
  ref <- newUniq
  a' <- tc (Infer ref) a
  t <- tGetRefType ref
  return (a', t)

tCheck :: forall a b . (Expected -> a -> T b) -> EType -> a -> T b
tCheck tc t = tc (Check t)

tInferExpr :: HasCallStack =>
              Expr -> T (Typed Expr)
tInferExpr = tInfer tcExpr

tCheckExpr :: HasCallStack =>
              EType -> Expr -> T Expr
tCheckExpr t e | Just (ctx, t') <- getImplies t = do
--  error $ "tCheckExpr: " ++ show (e, ctx, t')
  d <- newADictIdent (getSLoc e)
  e' <- withDict d ctx $ tCheckExpr t' e
  return $ eLam [EVar d] e'

tCheckExpr t e = tCheck tcExpr t e

tGetRefType :: HasCallStack =>
               TRef -> T EType
tGetRefType ref = do
  m <- gets uvarSubst
  case IM.lookup ref m of
    Nothing -> return (EUVar ref) -- error "tGetRefType"
    Just t  -> return t

-- Set the type for an Infer
tSetRefType :: HasCallStack =>
               SLoc -> TRef -> EType -> T ()
tSetRefType loc ref t = do
  m <- gets uvarSubst
  case IM.lookup ref m of
    Nothing -> putUvarSubst (IM.insert ref t m)
    Just tt -> unify loc tt t

-- Get the type of an already set Expected
tGetExpType :: Expected -> T EType
tGetExpType (Check t) = return t
tGetExpType (Infer r) = tGetRefType r

{-
-- Get the type of a possibly unset Expected
tGetExpTypeSet :: SLoc -> Expected -> T EType
tGetExpTypeSet _   (Check t) = return t
tGetExpTypeSet loc (Infer r) = tGetRefType r {-do
  t <- newUVar
  tSetRefType loc r t
  return t-}
-}

tcExpr :: HasCallStack =>
          Expected -> Expr -> T Expr
tcExpr mt ae = do
--  traceM ("tcExpr enter: " ++ showExpr ae)
  r <- tcExprR mt ae
--  traceM ("tcExpr exit: " ++ showExpr r)
  return r
tcExprR :: HasCallStack =>
           Expected -> Expr -> T Expr
tcExprR mt ae =
  let { loc = getSLoc ae } in
--  trace ("tcExprR " ++ show ae) $
  case ae of
    EVar i | isIdent dictPrefixDollar i -> do
             -- Magic variable that just becomes the dictionary
             d <- newIdent (getSLoc i) dictPrefixDollar
             case mt of
               Infer _ -> impossible
               Check t -> addConstraint d t
             return (EVar d)

           | isDummyIdent i -> impossibleShow ae
           | otherwise -> do
             -- Type checking an expression (or type)
             (e, t) <- tLookupV i
             -- Variables bound in patterns start out with an (EUVar ref) type,
             -- which can be instantiated to a polytype.
             -- Dereference such a ref.
             t' <-
               case t of
                 EUVar r -> fmap (fromMaybe t) (getUVar r)
                 _ -> return t
--             traceM ("EVar " ++ showIdent i ++ " :: " ++ showExpr t ++ " = " ++ showExpr t')
             instSigma loc e t' mt

    EApp f a -> do
      (f', ft) <- tInferExpr f
--      traceM $ "tcExpr(1) EApp f=" ++ show f ++ "; f'=" ++ show f' ++ " :: " ++ show ft
      (at, rt) <- unArrow loc ft
--      traceM $ "tcExpr(2) EApp: f=" ++ show f ++ " :: " ++ show ft ++ ", a=" ++ show a ++ " :: " ++ show at ++ " rt=" ++ show rt
      a' <- checkSigma a at
      instSigma loc (EApp f' a') rt mt

    EOper e ies -> do e' <- tcOper e ies; tcExpr mt e'
    ELam qs -> tcExprLam mt qs
    ELit loc' l -> do
      tcm <- gets tcMode
      case tcm of
        TCType ->
          case l of
            LStr _ -> instSigma loc' (ELit loc' l) (tConI loc' nameSymbol) mt
            LInteger _ -> instSigma loc' (ELit loc' l) (tConI loc' nameNat) mt
            _      -> impossible
        TCExpr -> do
          let getExpected (Infer _) = pure Nothing
              getExpected (Check t) = do
                t' <- derefUVar t >>= expandSyn
                case t' of
                  EVar v -> pure (Just v)
                  _      -> pure Nothing
          case l of
            LInteger i -> do
              mex <- getExpected mt
              case mex of
                -- Convert to Int in the compiler, that way (99::Int) will never involve fromInteger
                -- (which is not always in scope).
                Just v | v == mkIdent nameInt     -> tcLit  mt loc' (LInt (_integerToInt i))
                       | v == mkIdent nameWord    -> tcLit' mt loc' (LInt (_integerToInt i)) (tConI loc' nameWord)
                       | v == mkIdent nameDouble  -> tcLit  mt loc' (LDouble (_integerToDouble i))
                       | v == mkIdent nameInteger -> tcLit  mt loc' l
                _ -> do
                  (f, ft) <- tInferExpr (EVar (mkIdentSLoc loc' "fromInteger"))  -- XXX should have this qualified somehow
                  (_at, rt) <- unArrow loc ft
                  -- We don't need to check that _at is Integer, it's part of the fromInteger type.
                  instSigma loc (EApp f ae) rt mt
            LRat r -> do
              mex <- getExpected mt
              case mex of
                Just v | v == mkIdent nameDouble -> tcLit  mt loc' (LDouble (fromRational r))
                _ -> do
                  (f, ft) <- tInferExpr (EVar (mkIdentSLoc loc' "fromRational"))  -- XXX should have this qualified somehow
                  (_at, rt) <- unArrow loc ft
                  -- We don't need to check that _at is Rational, it's part of the fromRational type.
                  instSigma loc (EApp f ae) rt mt
            -- Not LInteger, LRat
            _ -> tcLit mt loc' l
        _ -> impossible
    ECase a arms -> do
      -- XXX should look more like EIf
      (ea, ta) <- tInferExpr a
      tt <- tGetExpType mt
      earms <- mapM (tcArm tt ta) arms
      return (ECase ea earms)
    ELet bs a -> tcBinds bs $ \ ebs -> do { ea <- tcExpr mt a; return (ELet ebs ea) }
    ETuple es -> do
      -- XXX checking if mt is a tuple would give better inference
      let
        n = length es
      (ees, tes) <- fmap unzip (mapM tInferExpr es)
      let
        ttup = tApps (tupleConstr loc n) tes
      munify loc mt ttup
      return (ETuple ees)
    EDo mmn ass -> do
      case ass of
        [] -> impossible
        [as] ->
          case as of
            SThen a -> tcExpr mt a
            _ -> tcError loc $ "bad final do statement"
        as : ss -> do
          case as of
            SBind p a -> do
              let
                -- XXX this wrong, it should be >>= from Monad
                ibind = mkIdentSLoc loc ">>="
                sbind = maybe ibind (\ mn -> qualIdent mn ibind) mmn
                x = eVarI loc "$b"
              tcExpr mt (EApp (EApp (EVar sbind) a)
                              (eLam [x] (ECase x [(p, EAlts [([], EDo mmn ss)] [])])))
            SThen a -> do
              let
                ithen = mkIdentSLoc loc ">>"
                sthen = maybe ithen (\ mn -> qualIdent mn ithen) mmn
              tcExpr mt (EApp (EApp (EVar sthen) a) (EDo mmn ss))
                
            SLet bs ->
              tcExpr mt (ELet bs (EDo mmn ss))

    ESectL e i -> tcExpr mt (EApp (EVar i) e)
    ESectR i e -> do
        let x = eVarI loc "$x"
        tcExpr mt (eLam [x] (EApp (EApp (EVar i) x) e))
    EIf e1 e2 e3 -> do
      e1' <- tCheckExpr (tBool (getSLoc e1)) e1
      case mt of
        Check t -> do
          e2' <- checkSigma e2 t
          e3' <- checkSigma e3 t
          return (EIf e1' e2' e3')
        Infer ref -> do
          (e2', t2) <- tInferExpr e2
          (e3', t3) <- tInferExpr e3
          e2'' <- subsCheck loc e2' t2 t3
          e3'' <- subsCheck loc e3' t3 t2
          tSetRefType loc ref t2
          return (EIf e1' e2'' e3'')

    EListish (LList es) -> do
      te <- newUVar
      munify loc mt (tApp (tList loc) te)
      es' <- mapM (tCheckExpr te) es
      return (EListish (LList es'))
    EListish (LCompr eret ass) -> do
      let
        doStmts :: [EStmt] -> [EStmt] -> T ([EStmt], Typed Expr)
        doStmts rss xs =
          case xs of
            [] -> do
              r <- tInferExpr eret
              return (reverse rss, r)
            as : ss ->
              case as of
                SBind p a -> do
                  v <- newUVar
                  ea <- tCheckExprAndSolve (tApp (tList loc) v) a
                  tCheckPatC v p $ \ ep -> doStmts (SBind ep ea : rss) ss
                SThen a -> do
                  ea <- tCheckExprAndSolve (tBool (getSLoc a)) a
                  doStmts (SThen ea : rss) ss
                SLet bs ->
                  tcBinds bs $ \ ebs ->
                    doStmts (SLet ebs : rss) ss
      (rss, (ea, ta)) <- doStmts [] ass
      let
        tr = tApp (tList loc) ta
      munify loc mt tr
      return (EListish (LCompr ea rss))
    EListish (LFrom       e)        -> tcExpr mt (enum loc "From" [e])
    EListish (LFromTo     e1 e2)    -> tcExpr mt (enum loc "FromTo" [e1, e2])
    EListish (LFromThen   e1 e2)    -> tcExpr mt (enum loc "FromThen" [e1,e2])
    EListish (LFromThenTo e1 e2 e3) -> tcExpr mt (enum loc "FromThenTo" [e1,e2,e3])
    ESign e t -> do
      t' <- tcType (Check kType) t
      e' <- instSigma loc e t' mt
      checkSigma e' t'
    -- Only happens in type&kind checking mode.
    EForall vks t ->
--      assertTCMode (==TCType) $
      withVks vks $ \ vks' -> do
        tt <- tcExpr mt t
        return (EForall vks' tt)
    EUpdate e ies -> do
      (e', _) <- tInferExpr e
      case e' of
        ECon c -> do
          let fs = conFields c
              is = map fst ies
              as = map field fs
              field i = fromMaybe (unsetField i) $ lookup i ies
          case is \\ fs of
            vs@(v:_) -> tcError (getSLoc v) $ "extra field(s) " ++ unwords (map unIdent vs)
            _ -> return ()
          tcExpr mt (foldl EApp e as)
        _ -> do
          let set = foldr eSetField e ies
          tcExpr mt set
    ESelect i -> tcExpr mt $ eGetField i
    _ -> error $ "tcExpr: cannot handle: " ++ show (getSLoc ae) ++ " " ++ show ae
      -- impossible

eSetField :: (Ident, Expr) -> Expr -> Expr
eSetField (i, e) r = EApp (EApp (EApp (EVar iset) (eProxy i)) r) e
  where iset = mkIdentSLoc (getSLoc i) "setField"

eGetField :: Ident -> Expr
eGetField i = EApp (EVar iget) (eProxy i)
  where iget = mkIdentSLoc (getSLoc i) "getField"

eProxy :: Ident -> Expr
eProxy i = ESign proxy (EApp proxy (ELit loc (LStr (unIdent i))))
  where proxy = EVar $ mkIdentSLoc loc "Proxy"
        loc = getSLoc i

-- XXX could be better
unsetField :: Ident -> Expr
unsetField i = EVar $ mkIdentSLoc (getSLoc i) "undefined"

enum :: SLoc -> String -> [Expr] -> Expr
enum loc f = foldl EApp (EVar (mkIdentSLoc loc ("enum" ++ f)))

tcLit :: Expected -> SLoc -> Lit -> T Expr
tcLit mt loc l@(LPrim _) = newUVar >>= tcLit' mt loc l
tcLit mt loc l = do
  let t =
        case l of
          LInt _     -> tConI loc nameInt
          LInteger _ -> tConI loc nameInteger
          LDouble _  -> tConI loc nameDouble
          LChar _    -> tConI loc nameChar
          LStr _     -> tApp (tList loc) (tConI loc nameChar)
          _          -> impossible
  tcLit' mt loc l t

tcLit' :: Expected -> SLoc -> Lit -> EType -> T Expr
tcLit' mt loc l t = instSigma loc (ELit loc l) t mt

-- tcOper is in T because it has to look up identifiers, and get the fixity table.
-- But there is no type checking happening here.
tcOper :: HasCallStack =>
          Expr -> [(Ident, Expr)] -> T Expr
tcOper ae aies = do
  fixs <- gets fixTable
  let
    opfix :: (Ident, Expr) -> T ((Expr, Fixity), Expr)
    opfix (i, e) = do
      (ei, _) <- tLookupV i
      let fx = getFixity fixs (getIdent ei)
      return ((EVar i, fx), e)

  ites <- mapM opfix aies
  case resolveFixity ae ites of
    Left (loc, err) -> tcError loc err
    Right e -> return e

unArrow :: HasCallStack =>
           SLoc -> EType -> T (EType, EType)
unArrow loc t = do
  case getArrow t of
    Just ar -> return ar
    Nothing -> do
      a <- newUVar
      r <- newUVar
      unify loc t (tArrow a r)
      return (a, r)

getFixity :: FixTable -> Ident -> Fixity
getFixity fixs i = fromMaybe (AssocLeft, 9) $ M.lookup i fixs

-- Dictionary argument names
adictPrefix :: String
adictPrefix = "adict"

newADictIdent :: SLoc -> T Ident
newADictIdent loc = newIdent loc adictPrefix

-- Needed dictionaries
dictPrefix :: String
dictPrefix = "dict"

dictPrefixDollar :: String
dictPrefixDollar = dictPrefix ++ uniqIdentSep

newDictIdent :: SLoc -> T Ident
newDictIdent loc = newIdent loc dictPrefix

tcExprLam :: Expected -> [Eqn] -> T Expr
tcExprLam mt qs = do
  t <- tGetExpType mt
  ELam <$> tcEqns False t qs

tcEqns :: Bool -> EType -> [Eqn] -> T [Eqn]
--tcEqns _ t eqns | trace ("tcEqns: " ++ showEBind (BFcn dummyIdent eqns) ++ " :: " ++ show t) False = undefined
tcEqns top (EForall iks t) eqns = withExtTyps iks $ tcEqns top t eqns
tcEqns top t eqns | Just (ctx, t') <- getImplies t = do
  let loc = getSLoc eqns
  d <- newADictIdent loc
  f <- newIdent loc "fcnD"
  withDict d ctx $ do
    eqns' <- tcEqns top t' eqns
    let eqn =
          case eqns' of
            [Eqn [] alts] -> Eqn [EVar d] alts
            _             -> Eqn [EVar d] $ EAlts [([], EVar f)] [BFcn f eqns']
    return [eqn]
tcEqns top t eqns = do
  let loc = getSLoc eqns
  f <- newIdent loc "fcnS"
  (eqns', ds) <- solveAndDefault top $ mapM (tcEqn t) eqns
--  traceM $ "tcEqns done: " ++ showEBind (BFcn dummyIdent eqns')
  case ds of
    [] -> return eqns'
    _  -> do
      let
        bs = eBinds ds
        eqn = Eqn [] $ EAlts [([], EVar f)] (bs ++ [BFcn f eqns'])
      return [eqn]

tcEqn :: EType -> Eqn -> T Eqn
--tcEqn t eqn | trace ("tcEqn: " ++ show eqn ++ " :: " ++ show t) False = undefined
tcEqn t eqn =
  case eqn of
    Eqn ps alts -> tcPats t ps $ \ t' ps' -> do
--      traceM $ "tcEqn " ++ show ps ++ " ---> " ++ show ps'
      alts' <- tcAlts t' alts
      return (Eqn ps' alts')

-- Only used above
tcPats :: EType -> [EPat] -> (EType -> [EPat] -> T Eqn) -> T Eqn
tcPats t [] ta = ta t []
tcPats t (p:ps) ta = do
  (tp, tr) <- unArrow (getSLoc p) t
  -- tCheckPatC dicts used in tcAlt solve
  tCheckPatC tp p $ \ p' -> tcPats tr ps $ \ t' ps' -> ta t' (p' : ps')

tcAlts :: EType -> EAlts -> T EAlts
tcAlts t (EAlts alts bs) =
--  trace ("tcAlts: bs in " ++ showEBinds bs) $
  tcBinds bs $ \ bs' -> do
--    traceM ("tcAlts: bs out " ++ showEBinds bbs)
    alts' <- mapM (tcAlt t) alts
    return (EAlts alts' bs')

tcAlt :: EType -> EAlt -> T EAlt
--tcAlt t (_, rhs) | trace ("tcAlt: " ++ showExpr rhs ++ " :: " ++ showEType t) False = undefined
tcAlt t (ss, rhs) = tcGuards ss $ \ ss' -> do
  rhs' <- tCheckExprAndSolve t rhs
  return (ss', rhs')

tcGuards :: [EStmt] -> ([EStmt] -> T EAlt) -> T EAlt
tcGuards [] ta = ta []
tcGuards (s:ss) ta = tcGuard s $ \ rs -> tcGuards ss $ \ rss -> ta (rs:rss)

tcGuard :: EStmt -> (EStmt -> T EAlt) -> T EAlt
tcGuard (SBind p e) ta = do
  (e', tt) <- tInferExpr e
  -- tCheckPatC dicts used in solving in tcAlt
  tCheckPatC tt p $ \ p' -> ta (SBind p' e')
tcGuard (SThen e) ta = do
  e' <- tCheckExprAndSolve (tBool (getSLoc e)) e
  ta (SThen e')
-- XXX do we have solves
tcGuard (SLet bs) ta = tcBinds bs $ \ bs' -> ta (SLet bs')

tcArm :: EType -> EType -> ECaseArm -> T ECaseArm
tcArm t tpat arm =
  case arm of
    -- The dicts introduced by tCheckPatC are
    -- used in the tCheckExprAndSolve in tcAlt.
    (p, alts) -> tCheckPatC tpat p $ \ pp -> do
      alts' <- tcAlts t alts
      return (pp, alts')

tCheckExprAndSolve :: EType -> Expr -> T Expr
tCheckExprAndSolve t e = do
  (e', bs) <- solveLocalConstraints $ tCheckExpr t e
  if null bs then
    return e'
   else
    return $ ELet (eBinds bs) e'

eBinds :: [(Ident, Expr)] -> [EBind]
eBinds ds = [BFcn i $ simpleEqn e | (i, e) <- ds]

instPatSigma :: HasCallStack =>
                 SLoc -> Sigma -> Expected -> T ()
instPatSigma loc pt (Infer r) = tSetRefType loc r pt
instPatSigma loc pt (Check t) = do { _ <- subsCheck loc undefined t pt; return () } -- XXX really?

subsCheck :: HasCallStack =>
              SLoc -> Expr -> Sigma -> Sigma -> T Expr
-- (subsCheck args off exp) checks that
-- 'off' is at least as polymorphic as 'args -> exp'
subsCheck loc exp1 sigma1 sigma2 = do -- Rule DEEP-SKOL
  (skol_tvs, rho2) <- skolemise sigma2
  exp1' <- subsCheckRho loc exp1 sigma1 rho2
  esc_tvs <- getFreeTyVars [sigma1,sigma2]
  let bad_tvs = filter (\ i -> elem i esc_tvs) skol_tvs
  when (not (null bad_tvs)) $
    tcErrorTK loc "Subsumption check failed"
  return exp1'

tCheckPatC :: forall a . EType -> EPat -> (EPat -> T a) -> T a
tCheckPatC t p@(EVar v) ta | not (isConIdent v) = do  -- simple special case
  withExtVals [(v, t)] $ ta p
tCheckPatC t app ta = do
--  traceM $ "tCheckPatC: " ++ show app ++ " :: " ++ show t
  let vs = patVars app
  multCheck vs
  env <- mapM (\ v -> (v,) <$> newUVar) vs
  withExtVals env $ do
    (_sks, ds, pp) <- tCheckPat t app
--    traceM ("tCheckPatC: " ++ show pp)
    () <- checkArity 0 pp
--    xt <- derefUVar t
--    traceM ("tCheckPatC ds=" ++ show ds ++ "t=" ++ show xt)
    -- XXX must check for leaking skolems
    withDicts ds $
      ta pp

type EPatRet = ([TyVar], [(Ident, EConstraint)], EPat)  -- skolems, dictionaries, pattern

tCheckPat :: EType -> EPat -> T EPatRet
tCheckPat = tCheck tcPat
tInferPat :: EPat -> T (Typed EPatRet)
tInferPat = tInfer tcPat

-- XXX Has some duplication with tcExpr
tcPat :: Expected -> EPat -> T EPatRet
tcPat mt ae =
  let { loc = getSLoc ae } in
  case ae of
    EVar i | isDummyIdent i -> do
               -- _ can be anything, so just ignore it
               _ <- tGetExpType mt
               return ([], [], ae)

           | isConIdent i -> do
               ipt <- tLookupV i
--               traceM (show ipt)
               case ipt of
                  -- Sanity check
                  (_, EForall _ (EForall _ _)) -> return ()
                  _ -> undefined
               (app, EForall avs apt) <- tInst' ipt
               (sks, spt) <- shallowSkolemise avs apt
               (d, p, pt) <-
                 case getImplies spt of
                   Nothing -> return ([], app, apt)
                   Just (ctx, pt') -> do
                     di <- newADictIdent loc
                     return ([(di, ctx)], EApp app (EVar di), pt')
                   
               -- We will only have an expected type for a non-nullary constructor
               pp <- case mt of
                       Check ext -> subsCheck loc p ext pt
                       Infer r   -> do { tSetRefType loc r pt; return p }
               return (sks, d, pp)

           | otherwise -> do
               -- All pattern variables are in the environment as
               -- type references.  Assign the reference the given type.
               ext <- tGetExpType mt
               (p, t) <- tLookupV i
               case t of
                 EUVar r -> tSetRefType loc r ext
                 _ -> impossible
               return ([], [], p)

    EOper e ies -> do e' <- tcOper e ies; tcPat mt e'

    EApp f a -> do
      ((skf, df, f'), ft) <- tInferPat f
--      traceM $ "tcPat: EApp f=" ++ showExpr f ++ "; e'=" ++ showExpr f' ++ " :: " ++ showEType ft
      (at, rt) <- unArrow loc ft
--      traceM ("tcPat EApp: " ++ showExpr f ++ " :: " ++ showEType ft)
      (ska, da, a') <- tCheckPat at a
      instPatSigma loc rt mt
      return (skf ++ ska, df ++ da, EApp f' a')
           
    ETuple es -> do
      let
        n = length es
      (xs, tes) <- fmap unzip (mapM tInferPat es)
      let
        (sks, ds, ees) = unzip3 xs
        ttup = tApps (tupleConstr loc n) tes
      munify loc mt ttup
      return (concat sks, concat ds, ETuple ees)

    EListish (LList es) -> do
      te <- newUVar
      munify loc mt (tApp (tList loc) te)
      xs <- mapM (tCheckPat te) es
      let (sks, ds, es') = unzip3 xs
      return (concat sks, concat ds, EListish (LList es'))

    ELit _ _ ->
      tcPat mt (EViewPat (EApp (EVar (mkIdentSLoc loc "==")) ae) (EVar (mkIdentSLoc loc "True")))

    ESign e t -> do
      t' <- tcType (Check kType) t
      instPatSigma loc t' mt
      tCheckPat t' e

    EAt i p -> do
      (_, ti) <- tLookupV i
      (sk, d, p') <- tcPat mt p
      tt <- tGetExpType mt
      case ti of
        EUVar r -> tSetRefType loc r tt
        _ -> impossible
      return (sk, d, EAt i p')

    EViewPat e p -> do
      (e', te) <- tInferExpr e
      (tea, ter) <- unArrow loc te
      munify loc mt tea
      (sk, d, p') <- tcPat (Check ter) p
      return (sk, d, EViewPat e' p')

    _ -> error $ "tcPat: " ++ show (getSLoc ae) ++ " " ++ show ae

multCheck :: [Ident] -> T ()
multCheck vs =
  when (anySame vs) $ do
    let v = head vs
    tcError (getSLoc v) $ "Multiply defined: " ++ showIdent v

checkArity :: Int -> EPat -> T ()
checkArity n (EApp f a) = do
  checkArity (n+1) f
  checkArity 0 a
checkArity n (ECon c) =
  let a = conArity c
  in  if n < a then
        tcError (getSLoc c) "too few arguments"
      else if n > a then
        tcError (getSLoc c) $ "too many arguments"
      else
        return ()
checkArity n (EAt _ p) = checkArity n p
checkArity n (ESign p _) = checkArity n p
checkArity n p =
  case p of
    ETuple _           -> check0
    EListish (LList _) -> check0
    EVar _             -> check0
    ELit _ _           -> check0
    ENegApp _          -> check0
    EViewPat _ _       -> check0
    _ -> impossible
  where
    check0 = if n /= 0 then tcError (getSLoc p) "Bad pattern" else return ()

tcBinds :: forall a . [EBind] -> ([EBind] -> T a) -> T a
tcBinds xbs ta = do
  let
    tmap = M.fromList [ (i, t) | BSign i t <- xbs ]
    xs = getBindsVars xbs
  multCheck xs
  xts <- mapM (tcBindVarT tmap) xs
  withExtVals xts $ do
    nbs <- mapM tcBind xbs
    ta nbs

tcBindVarT :: M.Map EType -> Ident -> T (Ident, EType)
tcBindVarT tmap x = do
  case M.lookup x tmap of
    Nothing -> do
      t <- newUVar
      return (x, t)
    Just t -> do
      tt <- withTypeTable $ tcTypeT (Check kType) t
      return (x, tt)

tcBind :: EBind -> T EBind
tcBind abind =
  case abind of
    BFcn i eqns -> do
      (_, tt) <- tLookupV i
      teqns <- tcEqns False tt eqns
      return $ BFcn i teqns
    BPat p a -> do
      ((sk, ds, ep), tp) <- tInferPat p  -- pattern variables already bound
      -- This is just to complicated.
      when (not (null sk) || not (null ds)) $
        tcError (getSLoc p) "existentials not allowed in pattern binding"
      ea <- tCheckExprAndSolve tp a
      return $ BPat ep ea
    BSign _ _ -> return abind

-- Desugar [T] and (T,T,...)
dsType :: EType -> EType
dsType at =
  case at of
    EVar _ -> at
    EApp f a -> EApp (dsType f) (dsType a)
    EOper t ies -> EOper (dsType t) [(i, dsType e) | (i, e) <- ies]
    EListish (LList [t]) -> tApp (tList (getSLoc at)) (dsType t)
    ETuple ts -> tApps (tupleConstr (getSLoc at) (length ts)) (map dsType ts)
    ESign t k -> ESign (dsType t) k
    EForall iks t -> EForall iks (dsType t)
    ELit _ (LStr _) -> at
    ELit _ (LInteger _) -> at
    _ -> impossible

tConI :: SLoc -> String -> EType
tConI loc = tCon . mkIdentSLoc loc

tListI :: SLoc -> Ident
tListI loc = mkIdentSLoc loc $ listPrefix ++ "[]"

tList :: SLoc -> EType
tList = tCon . tListI

tBool :: SLoc -> EType
tBool loc = tConI loc $ boolPrefix ++ "Bool"

impossible :: forall a .
              HasCallStack =>
              a
impossible = error "impossible"

impossibleShow :: forall a b .
                  HasCallStack =>
                  (Show a, HasLoc a) => a -> b
impossibleShow a = error $ "impossible: " ++ show (getSLoc a) ++ " " ++ show a

showTModule :: forall a . (a -> String) -> TModule a -> String
showTModule sh amdl =
  case amdl of
    TModule mn _ _ _ _ _ _ a -> "Tmodule " ++ showIdent mn ++ "\n" ++ sh a ++ "\n"

-----------------------------------------------------

getFreeTyVars :: [EType] -> T [TyVar]
getFreeTyVars tys = do
  tys' <- mapM derefUVar tys
  return (freeTyVars tys')

getMetaTyVars :: [EType] -> T [TRef]
getMetaTyVars tys = do
  tys' <- mapM derefUVar tys
  return (metaTvs tys')

getEnvTypes :: T [EType]
getEnvTypes = gets (map entryType . stElemsLcl . valueTable)

{-
quantify :: [MetaTv] -> Rho -> T Sigma
-- Quantify over the specified type variables (all flexible)
quantify tvs ty = do
   mapM_ bind (tvs `zip` new_bndrs) -- 'bind' is just a cunning way
   ty' <- zonkType ty               -- of doing the substitution
   return (EForall new_bndrs_kind ty')
  where
    used_bndrs = tyVarBndrs ty -- Avoid quantified type variables in use
    new_bndrs = allBinders \\ used_bndrs
    bind (tv, name) = writeTcRef tv (EVar name)
    new_bndrs_kind = map (\ i -> IdKind i undefined) new_bndrs

allBinders :: [Ident] -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders = [ mkIdent [chr x] | x <- [ord 'a' .. ord 'z'] ] ++
             [ mkIdent (chr x : show i) | i <- [1 ..], x <- [ord 'a' .. ord 'z']]
-}

-- Skolemize the given variables
shallowSkolemise :: [IdKind] -> EType -> T ([TyVar], EType)
shallowSkolemise tvs ty = do
  sks <- mapM (newSkolemTyVar . idKindIdent) tvs
  return (sks, subst (zip (map idKindIdent tvs) (map EVar sks)) ty)

skolemise :: HasCallStack =>
             Sigma -> T ([TyVar], Rho)
-- Performs deep skolemisation, returning the
-- skolem constants and the skolemised type.
skolemise (EForall tvs ty) = do -- Rule PRPOLY
  (sks1, ty') <- shallowSkolemise tvs ty
  (sks2, ty'') <- skolemise ty'
  return (sks1 ++ sks2, ty'')
skolemise t@(EApp _ _) | Just (arg_ty, res_ty) <- getArrow t = do
  (sks, res_ty') <- skolemise res_ty
  return (sks, arg_ty `tArrow` res_ty')
skolemise (EApp f a) = do
  (sks1, f') <- skolemise f
  (sks2, a') <- skolemise a
  return (sks1 ++ sks2, EApp f' a')
skolemise ty =
  return ([], ty)

-- Skolem tyvars are just identifiers that start with a uniq
newSkolemTyVar :: Ident -> T Ident
newSkolemTyVar tv = do
  uniq <- newUniq
  return (mkIdentSLoc (getSLoc tv) (unIdent tv ++ "#" ++ show uniq))

freeTyVars :: [EType] -> [TyVar]
-- Get the free TyVars from a type; no duplicates in result
freeTyVars = foldr (go []) []
  where
    go :: [TyVar] -- Ignore occurrences of bound type variables
       -> EType   -- Type to look at
       -> [TyVar] -- Accumulates result
       -> [TyVar]
    go bound (EVar tv) acc
      | elem tv bound = acc
      | elem tv acc = acc
      | isConIdent tv = acc
      | otherwise = tv : acc
    go bound (EForall tvs ty) acc = go (map idKindIdent tvs ++ bound) ty acc
    go bound (EApp fun arg) acc = go bound fun (go bound arg acc)
    go _bound (EUVar _) acc = acc
    go _ _ _ = undefined

metaTvs :: [EType] -> [TRef]
-- Get the MetaTvs from a type; no duplicates in result
metaTvs tys = foldr go [] tys
  where
    go (EUVar tv) acc
      | elem tv acc = acc
      | otherwise = tv : acc
    go (EVar _) acc = acc
    go (EForall _ ty) acc = go ty acc
    go (EApp fun arg) acc = go fun (go arg acc)
    go (ELit _ _) acc = acc
    go _ _ = impossible

{-
tyVarBndrs :: Rho -> [TyVar]
-- Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs ty = nub (bndrs ty)
  where
    bndrs (EForall tvs body) = map idKindIdent tvs ++ bndrs body
    bndrs (EApp arg res) = bndrs arg ++ bndrs res
    bndrs (EVar _) = []
    bndrs _ = undefined

inferSigma :: Expr -> T (Expr, Sigma)
inferSigma e = do
  (e', exp_ty) <- inferRho e
  env_tys      <- getEnvTypes
  env_tvs      <- getMetaTyVars env_tys
  res_tvs      <- getMetaTyVars [exp_ty]
  let forall_tvs = res_tvs \\ env_tvs
  (e',) <$> quantify forall_tvs exp_ty
-}

checkSigma :: HasCallStack =>
              Expr -> Sigma -> T Expr
checkSigma expr sigma = do
  (skol_tvs, rho) <- skolemise sigma
  expr' <- tCheckExpr rho expr
  if null skol_tvs then
    -- Fast special case
    return expr'
   else do
    env_tys <- getEnvTypes
    esc_tvs <- getFreeTyVars (sigma : env_tys)
    let bad_tvs = filter (\ i -> elem i esc_tvs) skol_tvs
    when (not (null bad_tvs)) $
      tcErrorTK (getSLoc expr) $ "not polymorphic enough: " ++ unwords (map showIdent bad_tvs)
    return expr'

subsCheckRho :: HasCallStack =>
                SLoc -> Expr -> Sigma -> Rho -> T Expr
--subsCheckRho _ e1 t1 t2 | trace ("subsCheckRho: " ++ show e1 ++ " :: " ++ show t1 ++ " = " ++ show t2) False = undefined
subsCheckRho loc exp1 sigma1@(EForall _ _) rho2 = do -- Rule SPEC
  (exp1', rho1) <- tInst (exp1, sigma1)
  subsCheckRho loc exp1' rho1 rho2
subsCheckRho loc exp1 arho1 rho2 | Just _ <- getImplies arho1 = do
  (exp1', rho1) <- tInst (exp1, arho1)
  subsCheckRho loc exp1' rho1 rho2
subsCheckRho loc exp1 rho1 rho2 | Just (a2, r2) <- getArrow rho2 = do -- Rule FUN
  (a1, r1) <- unArrow loc rho1
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 rho1 rho2 | Just (a1, r1) <- getArrow rho1 = do -- Rule FUN
  (a2,r2) <- unArrow loc rho2
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 tau1 tau2 = do  -- Rule MONO
  unify loc tau1 tau2 -- Revert to ordinary unification
  return exp1

subsCheckFun :: HasCallStack =>
                SLoc -> Expr -> Sigma -> Rho -> Sigma -> Rho -> T Expr
subsCheckFun loc e1 a1 r1 a2 r2 = do
  _ <- subsCheck loc undefined a2 a1   -- XXX
  subsCheckRho loc e1 r1 r2

instSigma :: HasCallStack =>
             SLoc -> Expr -> Sigma -> Expected -> T Expr
instSigma loc e1 t1 (Check t2) = do
--  traceM ("instSigma: Check " ++ showEType t1 ++ " = " ++ showEType t2)
  subsCheckRho loc e1 t1 t2
instSigma loc e1 t1 (Infer r) = do
  (e1', t1') <- tInst (e1, t1)
--  traceM ("instSigma: Infer " ++ showEType t1 ++ " ==> " ++ showEType t1')
  tSetRefType loc r t1'
  return e1'

-----

-- Given a dictionary of a (constraint type), split it up
--  * name components of a tupled constraint
--  * name superclasses of a constraint
expandDict :: HasCallStack => Expr -> EConstraint -> T [InstDictC]
expandDict edict ct = expandDict' [] [] edict =<< expandSyn ct

expandDict' :: [IdKind] -> [EConstraint] -> Expr -> EConstraint -> T [InstDictC]
expandDict' avks actx edict acc = do
  let
    (bvks, bctx, cc) = splitInst acc
    (iCls, args) = getApp cc
    vks = avks ++ bvks
    ctx = actx ++ bctx
  case getTupleConstr iCls of
    Just _ -> do
      concat <$> mapM (\ (i, a) -> expandDict' vks ctx (mkTupleSel i (length args) `EApp` edict) a) (zip [0..] args)
    Nothing -> do
      ct <- gets classTable
      case M.lookup iCls ct of
        Nothing -> do
          -- if iCls is a variable it's not in the class table, otherwise it's an error
          when (isConIdent iCls) $
            impossible
          return [(edict, vks, ctx, cc, [])]
        Just (iks, sups, _, _, fds) -> do
          let 
            vs = map idKindIdent iks
            sub = zip vs args
            sups' = map (subst sub) sups
          insts <- concat <$> mapM (\ (i, sup) -> expandDict' vks ctx (EVar (mkSuperSel iCls i) `EApp` edict) sup) (zip [1 ..] sups')
          return $ (edict, vks, ctx, cc, fds) : insts

mkSuperSel :: HasCallStack =>
              Ident -> Int -> Ident
mkSuperSel c i = addIdentSuffix c ("$super" ++ show i)

---------------------------------

type Solved = (Ident, Expr)

-- Solve constraints generated locally in 'ta'.
-- Keep any unsolved ones for later.
solveLocalConstraints :: forall a . T a -> T (a, [Solved])
solveLocalConstraints ta = do
  cs <- gets constraints           -- old constraints
  putConstraints []                -- start empty
  a <- ta                          -- compute, generating constraints
  ds <- solveConstraints           -- solve those
  un <- gets constraints           -- get remaining unsolved
  putConstraints (un ++ cs)        -- put back unsolved and old constraints
  return (a, ds)

solveAndDefault :: forall a . Bool -> T a -> T (a, [Solved])
solveAndDefault False ta = solveLocalConstraints ta
solveAndDefault True  ta = do
  a <- ta
  ds <- solveConstraints
  cs <- gets constraints
  vs <- getMetaTyVars (map snd cs)    -- These are the type variables that need defaulting
--  traceM $ "solveAndDefault: meta=" ++ show vs
  -- XXX may have to iterate this with fundeps
  ds' <- concat <$> mapM defaultOneTyVar vs
  return (a, ds ++ ds')

constraintHasTyVar :: TRef -> (Ident, EConstraint) -> T Bool
constraintHasTyVar tv (_, t) = elem tv <$> getMetaTyVars [t]

defaultOneTyVar :: TRef -> T [Solved]
defaultOneTyVar tv = do
  old <- get             -- get entire old state
  -- split constraints into those with the current tyvar and those without
  (ourcs, othercs) <- partitionM (constraintHasTyVar tv) (constraints old)
  let tryDefaults [] = return []
      tryDefaults (ty:tys) = do
        setUVar tv ty
        putConstraints ourcs
        ds <- solveConstraints
        rcs <- gets constraints
        if null rcs then do
          -- Success, the type variable is gone
          putConstraints othercs   -- put back the other constraints
          return ds
         else do
          -- Not solved, try with the nest type
          put old            -- restore solver state
          tryDefaults tys    -- and try with next type
  tryDefaults (defaults old)

{-
showInstInfo :: InstInfo -> String
showInstInfo (InstInfo m ds fds) = "InstInfo " ++ show (M.toList m) ++ " " ++ showListS showInstDict ds ++ show fds

showInstDict :: InstDict -> String
showInstDict (e, ctx, ts) = showExpr e ++ " :: " ++ show (addConstraints ctx (tApps (mkIdent "_") ts))

showInstDef :: InstDef -> String
showInstDef (cls, InstInfo m ds _) = "instDef " ++ show cls ++ ": "
            ++ show (M.toList m) ++ ", " ++ showListS showInstDict ds

showConstraint :: (Ident, EConstraint) -> String
showConstraint (i, t) = show i ++ " :: " ++ show t

showMatch :: (Expr, [EConstraint]) -> String
showMatch (e, ts) = show e ++ " " ++ show ts
-}

type Goal = (Ident, EType)     -- What we want to solve
type UGoal = Goal              -- Unsolved goal
type Soln = (Ident, Expr)      -- Solution, i.e., binding of a dictionary
type Improve = (SLoc, EType, EType)  -- Unify to get an improvement substitution

-- Solve as many constraints as possible.
-- Return bindings for the dictionary witnesses.
solveConstraints :: T [Soln]
solveConstraints = do
  cs <- gets constraints
  if null cs then
    return []
   else do
    addMetaDicts
--    traceM "------------------------------------------\nsolveConstraints"
    cs' <- mapM (\ (i,t) -> do { t' <- derefUVar t; return (i,t') }) cs
--    traceM ("constraints:\n" ++ unlines (map showConstraint cs'))
    (unsolved, solved, improves) <- solveMany cs' [] [] []
    putConstraints unsolved
--    traceM ("solved:\n"   ++ unlines [ showIdent i ++ " = "  ++ showExpr  e | (i, e) <- solved ])
--    traceM ("unsolved:\n" ++ unlines [ showIdent i ++ " :: " ++ showEType t | (i, t) <- unsolved ])
    if null improves then
      return solved
     else do
      -- We have improving substitutions.
      -- Do the unifications, and try to solve more.
      mapM_ (\ (l, a, b) -> unify l a b) improves
      (++ solved) <$> solveConstraints

-- A solver get a location, class&types (i.e. (C t1 ... tn)),
-- and, if successful, returns a dictionary expression and new goals.
type SolveOne = SLoc -> Ident -> [EType] -> T (Maybe (Expr, [Goal], [Improve]))

-- Table of constraint solvers.
-- The predicate gets the class name and picks a solver.
-- There must always by at least one solver that matches
solvers :: [(Ident -> Bool, SolveOne)]
solvers =
  [ (isJust . getTupleConstr,      solveTuple)        -- handle tuple constraints, i.e. (C1 t1, C2 t2, ...)
  , ((== mkIdent nameTypeEq),      solveTypeEq)       -- handle equality constraints, i.e. (t1 ~ t2)
  , ((== mkIdent nameKnownNat),    solveKnownNat)     -- KnownNat 999 constraints
  , ((== mkIdent nameKnownSymbol), solveKnownSymbol)  -- KnownNat 999 constraints
  , (const True,                   solveInst)         -- handle constraints with instances
  ]

-- Examine each goal, either solve it (possibly producing new goals) or let it remain unsolved.
solveMany :: [Goal] -> [UGoal] -> [Soln] -> [Improve] -> T ([UGoal], [Soln], [Improve])
solveMany [] uns sol imp = return (uns, sol, imp)
solveMany (cns@(di, ct) : cnss) uns sol imp = do
--  traceM ("trying " ++ showEType ct)
  let loc = getSLoc di
      (iCls, cts) = getApp ct
      solver = head [ s | (p, s) <- solvers, p iCls ]
  msol <- solver loc iCls cts
  case msol of
    Nothing           -> solveMany        cnss  (cns : uns)            sol         imp
    Just (de, gs, is) -> solveMany (gs ++ cnss)        uns ((di, de) : sol) (is ++ imp)

solveInst :: SolveOne
solveInst loc iCls cts = do
  it <- gets instTable
--  traceM ("instances:\n" ++ unlines (map showInstDef (M.toList it)))
  case M.lookup iCls it of
    Nothing -> return Nothing   -- no instances, so no chance
    Just (InstInfo atomMap insts fds) ->
      case cts of
        [EVar i] -> do
          case M.lookup i atomMap of
            -- If the goal is just (C T) and there is an instance, the solution is simple
            Just e  -> return $ Just (e, [], [])
            -- Not found, but there might be a generic instance
            Nothing -> solveGen fds insts loc iCls cts
        _           -> solveGen fds insts loc iCls cts

solveGen :: [IFunDep] -> [InstDict] -> SolveOne
solveGen fds insts loc iCls cts = do
--  traceM ("solveGen " ++ showEType ct)
  let matches = getBestMatches $ findMatches loc fds insts cts
--  traceM ("matches " ++ showListS showMatch matches)
  case matches of
    []              -> return Nothing
    [(de, ctx, is)] ->
      if null ctx then
        return $ Just (de, [], is)
      else do
        d <- newDictIdent loc
--        traceM ("constraint " ++ showIdent di ++ " :: " ++ showEType ct ++ "\n" ++
--                "   turns into " ++ showIdent d ++ " :: " ++ showEType (tupleConstraints ctx) ++ ", " ++
--                showIdent di ++ " = " ++ showExpr (EApp de (EVar d)))
        return $ Just (EApp de (EVar d), [(d, tupleConstraints ctx)], is)
    _ -> tcError loc $ "Multiple constraint solutions for: " ++ showEType (tApps iCls cts)
--                     ++ show (map fst matches)

-- Split a tupled contraint into its parts.
-- XXX should look for a direct (tupled) dictionary
solveTuple :: SolveOne
solveTuple loc _iCls cts = do
  goals <- mapM (\ c -> do { d <- newDictIdent loc; return (d, c) }) cts
  return $ Just (ETuple (map (EVar . fst) goals), goals, [])

solveTypeEq :: SolveOne
solveTypeEq loc _iCls [t1, t2] = do
  eqs <- gets typeEqTable
  case solveEq eqs t1 t2 of
    Nothing -> return Nothing
    Just (de, tts) -> do
      let mkEq (u1, u2) = do
            i <- newDictIdent loc
            return (i, mkEqType loc u1 u2)
      ncs <- mapM mkEq tts
      return $ Just (de, ncs, [])
solveTypeEq _ _ _ = impossible

solveKnownNat :: SolveOne
solveKnownNat loc iCls [e@(ELit _ (LInteger _))] = mkConstDict loc iCls e
solveKnownNat loc iCls ts = solveInst loc iCls ts  -- look for a dict argument

solveKnownSymbol :: SolveOne
solveKnownSymbol loc iCls [e@(ELit _ (LStr _))] = mkConstDict loc iCls e
solveKnownSymbol loc iCls ts = solveInst loc iCls ts  -- look for a dict argument

mkConstDict :: SLoc -> Ident -> Expr -> T (Maybe (Expr, [Goal], [Improve]))
mkConstDict loc iCls e = do
  let res = EApp (EVar $ mkClassConstructor iCls) fcn
      fcn = EApp (ELit loc (LPrim "K")) e                -- constant function
  return $ Just (res, [], [])

type TySubst = [(TRef, EType)]

-- Given some instances and a constraint, find the matching instances.
-- For each matching instance return: (subst-size, (dict-expression, new-constraints))
-- The subst-size is the size of the substitution that made the input instance match.
-- It is a measure of how exact the match is.
findMatches :: SLoc -> [IFunDep] -> [InstDict] -> [EType] -> [(Int, (Expr, [EConstraint], [Improve]))]
findMatches loc fds ds its =
 let rrr =
       [ (length s, (de, map (substEUVar s) ctx, imp))
       | (de, ctx, ts) <- ds, Just (s, imp) <- [matchTypes loc ts its fds] ]
 in --trace ("findMatches: " ++ showListS showInstDict ds ++ "; " ++ show its ++ "; " ++ show fds ++ "; " ++ show rrr)
    rrr

-- Do substitution for EUVar.
-- XXX similar to derefUVar
substEUVar :: TySubst -> EType -> EType
substEUVar [] t = t
substEUVar _ t@(EVar _) = t
substEUVar _ t@(ELit _ _) = t
substEUVar s (EApp f a) = EApp (substEUVar s f) (substEUVar s a)
substEUVar s t@(EUVar i) = fromMaybe t $ lookup i s
substEUVar s (EForall iks t) = EForall iks (substEUVar s t)
substEUVar _ _ = impossible

-- Length of lists match, because of kind correctness.
-- fds is a non-empty list.
matchTypes :: SLoc -> [EType] -> [EType] -> [IFunDep] -> Maybe (TySubst, [Improve])
matchTypes _ ats ats' [] = do
  -- Simple special case when there are no fundeps.
  let loop r (t:ts) (t':ts') = matchType r t t' >>= \ r' -> loop r' ts ts'
      loop r _ _ = pure r
  s <- loop [] ats ats'
  pure (s, [])
matchTypes loc ts ts' fds = asum $ map (matchTypesFD loc ts ts') fds

matchTypesFD :: SLoc -> [EType] -> [EType] -> IFunDep -> Maybe (TySubst, [Improve])
matchTypesFD loc ts ts' (ins, outs) = do
  let matchFD :: Bool -> EType -> EType -> Maybe TySubst
      matchFD True  = \ _ _ -> Just []     -- if it's an output, don't match
      matchFD False = matchType []          -- match types for non-outputs
  tms <- sequence $ zipWith3 matchFD outs ts ts'
  tm  <- combineTySubsts tms               -- combine all substitutions
  is  <- combineTySubsts [ s | (True, s) <- zip ins tms]  -- subst from input FDs
  let imp = [ (loc, substEUVar is t, t') | (True, t, t') <- zip3 outs ts ts' ]  -- improvements
  -- We don't allow output FDs to have tyvars that are not instantiated
--  traceM $ "matchTypesFD: " ++ show (ts, ts') ++ show (ins, outs) ++ show (tm, imp)
  when (not (null (metaTvs [ t | (_,t,_) <- imp ]))) $
    errorMessage loc $ "free type variable in output fundep"
  pure (tm, imp)

-- Match two types, instantiate variables in the first type.
matchType :: TySubst -> EType -> EType -> Maybe TySubst
matchType = match
  where
    match r (EVar i)   (EVar i')   | i == i' = pure r
    match r (ELit _ l) (ELit _ l') | l == l' = pure r
    match r (EApp f a) (EApp f' a') = do
      r' <- match r f f'
      match r' a a'
    match r (EUVar i) t' =
      -- For a variable, check that any previous match is the same.
      case lookup i r of
        Just t  -> match r t t'
        Nothing -> pure ((i, t') : r)
    match _ _ _ = Nothing

-- XXX This shouldn't be this complicated.
combineTySubsts :: [TySubst] -> Maybe TySubst
combineTySubsts = combs []
  where
    combs r [] = Just r
    combs r (s:ss) = do { r' <- comb r s; combs r' ss }
    comb :: TySubst -> TySubst -> Maybe TySubst
    comb r [] = Just r
    comb r ((v, t):s) = do { r' <- comb1 v t r; comb r' s }
    comb1 v t r =
      case lookup v r of
        Nothing -> Just ((v, t) : r)
        Just t' -> matchType [] t' t

-- Get the best matches.  These are the matches with the smallest substitution.
getBestMatches :: [(Int, (Expr, [EConstraint], [Improve]))] -> [(Expr, [EConstraint], [Improve])]
getBestMatches [] = []
getBestMatches ams =
  let (args, insts) = partition (\ (_, (ei, _, _)) -> (adictPrefix ++ uniqIdentSep) `isPrefixOf` unIdent (unvar ei)) ams
      unvar (EVar i) = i
      unvar e = impossibleShow e
      pick ms =
        let b = minimum (map fst ms)         -- minimum substitution size
        in  [ ec | (s, ec) <- ms, s == b ]   -- pick out the smallest
  in  if null args then pick insts else pick args

-- Check that there are no unsolved constraints.
checkConstraints :: HasCallStack => T ()
checkConstraints = do
  cs <- gets constraints
  case cs of
    [] -> return ()
    (i, t) : _ -> do
      t' <- derefUVar t
--      is <- gets instTable
--      traceM $ "Cannot satisfy constraint: " ++ unlines (map (\ (i, ii) -> show i ++ ":\n" ++ showInstInfo ii) (M.toList is))
      tcError (getSLoc i) $ "Cannot satisfy constraint: " ++ show t'

-- Add a type equality constraint.
addEqConstraint :: SLoc -> EType -> EType -> T ()
addEqConstraint loc t1 t2 = do
  d <- newDictIdent loc
  addConstraint d (mkEqType loc t1 t2)

mkEqType :: SLoc -> EType -> EType -> EConstraint
mkEqType loc t1 t2 = EApp (EApp (EVar (mkIdentSLoc loc nameTypeEq)) t1) t2

-- Possibly solve a type equality.
solveEq :: TypeEqTable -> EType -> EType -> Maybe (Expr, [(EType, EType)])
--solveEq eqs t1 t2 | trace ("solveEq: " ++ show (t1,t2) ++ show eqs) False = undefined
solveEq eqs t1 t2 | t1 `eqEType` t2            = Just (ETuple [], [])
                  | elemBy eqTyTy (t1, t2) eqs = Just (ETuple [], [])
                  | otherwise =
  case (t1, t2) of
    (EApp f1 a1, EApp f2 a2) -> Just (ETuple [], [(f1, f2), (a1, a2)])
    _                        -> Nothing

-- Add the equality t1~t2.
-- The type table is always the symmetric&transitive closure of all equalities.
-- This isn't very efficient, but it's simple.
addTypeEq :: EType -> EType -> TypeEqTable -> TypeEqTable
addTypeEq t1 t2 aeqs | t1 `eqEType` t2 || elemBy eqTyTy (t1, t2) aeqs || elemBy eqTyTy (t2, t1) aeqs = aeqs
                     | otherwise = (t1, t2) : (t2, t1) :                    -- symmetry
                                   trans t1 t2 aeqs ++ trans t2 t1 aeqs ++  -- transitivity
                                   aeqs
  where trans a1 a2 eqs = [ ab | (b1, b2) <- eqs, eqEType a2 b1, ab <- [(a1, b2), (b2, a1)] ]

eqTyTy :: (EType, EType) -> (EType, EType) -> Bool
eqTyTy (t1, t2) (u1, u2) = eqEType t1 u1 && eqEType t2 u2

---------------------

-- Try adding all dictionaries that used to have meta variables.
addMetaDicts :: T ()
addMetaDicts = do
  ms <- gets metaTable
  putMetaTable []
  mapM_ addDict ms  -- Try adding them

-----------------------------
{-
showSymTab :: SymTab Entry -> String
showSymTab (SymTab im ies) = showListS showIdent (map fst (M.toList im) ++ map fst ies)

showTModuleExps :: TModule a -> String
showTModuleExps (TModule mn _fxs tys _syns _clss _insts vals _defs) =
  showIdent mn ++ ":\n" ++
    unlines (map (("  " ++) . showValueExport) vals) ++
    unlines (map (("  " ++) . showTypeExport)  tys)

showValueExport :: ValueExport -> String
showValueExport (ValueExport i (Entry qi t)) =
  showIdent i ++ " = " ++ showExpr qi ++ " :: " ++ showEType t

showTypeExport :: TypeExport -> String
showTypeExport (TypeExport i (Entry qi t) vs) =
  showIdent i ++ " = " ++ showExpr qi ++ " :: " ++ showEType t ++ " assoc=" ++ showListS showValueExport vs

showIdentClassInfo :: (Ident, ClassInfo) -> String
showIdentClassInfo (i, (_vks, _ctx, cc, ms)) =
  showIdent i ++ " :: " ++ showEType cc ++
    " has " ++ showListS showIdent ms
-}

