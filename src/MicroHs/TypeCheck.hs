-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
module MicroHs.TypeCheck(
  typeCheck,
  TModule(..), showTModule,
  impossible,
  mkClassConstructor,
  mkSuperSel,
  bindingsOf,
  ) where
import Prelude --Xhiding(showList)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified MicroHs.State as S
import MicroHs.TCMonad as T
import qualified MicroHs.IdentMap as M
import MicroHs.Ident
import MicroHs.Expr
--Ximport Compat
--Ximport GHC.Stack
--Ximport Debug.Trace

data TModule a = TModule
  IdentModule     -- module names
  [FixDef]        -- all fixities, exported or not
  [TypeExport]    -- exported types
  [SynDef]        -- all type synonyms, exported or not
  [ClsDef]        -- all classes
  [InstDef]       -- all instances
  [ValueExport]   -- exported values (including from T(..))
  a               -- bindings
  --Xderiving (Show)

bindingsOf :: forall a . TModule a -> a
bindingsOf (TModule _ _ _ _ _ _ _ a) = a

data TypeExport = TypeExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  [ValueExport]   -- associated values, i.e., constructors, selectors, methods
  --Xderiving (Show)

data ValueExport = ValueExport
  Ident           -- unqualified name
  Entry           -- symbol table entry
  --Xderiving (Show)

type FixDef = (Ident, Fixity)
type SynDef = (Ident, EType)
type ClsDef = (Ident, ClassInfo)
type InstDef= (Ident, InstInfo)

type ClassInfo = ([IdKind], [EConstraint], [Ident])  -- class tyvars, superclasses, methods

-- Symbol table entry for symbol i.
data Entry = Entry
  Expr             -- convert (EVar i) to this expression; sometimes just (EVar i)
  EType            -- type/kind of identifier
  --Xderiving(Show)

entryType :: Entry -> EType
entryType (Entry _ t) = t

type ValueTable = M.Map [Entry]    -- type of value identifiers, used during type checking values
type TypeTable  = M.Map [Entry]    -- kind of type  identifiers, used during kind checking types
type KindTable  = M.Map [Entry]    -- sort of kind  identifiers, used during sort checking kinds
type SynTable   = M.Map EType      -- body of type synonyms
type FixTable   = M.Map Fixity     -- precedence and associativity of operators
type AssocTable = M.Map [Ident]    -- maps a type identifier to its associated construcors/selectors/methods
type ClassTable = M.Map ClassInfo  -- maps a class identifier to its associated information
type InstTable  = M.Map InstInfo   -- indexed by class name
type Constraints= [(Ident, EConstraint)]

-- To make type checking fast it is essential to solve constraints fast.
-- The naive implementation of InstInfo would be [InstDict], but
-- that is slow.
-- Instead, the data structure is specialized
--  * For single parameter type classes for atomic types, e.g., Eq Int
--    we use the type name (i.e., Int) to index into a map that gives
--    the dictionary directly.  This map is also used for dictionary arguments
--    of type, e.g., Eq a.
--  * NOT IMPLEMENTED: look up by type name of the left-most type
--  * As a last resort, just look through dictionaries.
data InstInfo = InstInfo
       (M.Map Expr)               -- map for direct lookup of atomic types
       [InstDict]                 -- slow path
  --Xderiving (Show)

-- This is the dictionary express, instance variables, instance context,
-- and class&types.
type InstDict   = (Expr, [IdKind], [EConstraint], EConstraint)

type Sigma = EType
--type Tau   = EType
type Rho   = EType
type TyVar = Ident

typeCheck :: forall a . [(ImportSpec, TModule a)] -> EModule -> TModule [EDef]
typeCheck aimps (EModule mn exps defs) =
--  trace (show amdl) $
  let
    imps = map filterImports aimps
    (fs, ts, ss, cs, is, vs, as) = mkTables imps
  in case tcRun (tcDefs defs) (initTC mn fs ts ss cs is vs as) of
       (tds, tcs) ->
         let
           thisMdl = (mn, mkTModule tds tcs)
           impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ m mm _, tm) <- imps]
           impMap = M.fromList [(i, m) | (i, m) <- thisMdl : impMdls]
           (texps, vexps) =
             unzip $ map (getTVExps impMap (typeTable tcs) (valueTable tcs) (assocTable tcs)) exps
           fexps = [ fe | TModule _ fe _ _ _ _ _ _ <- M.elems impMap ]
           sexps = M.toList (synTable tcs)
           cexps = [ ce | TModule _ _ _ _ ce _ _ _ <- M.elems impMap ]
           iexps = M.toList (instTable tcs)
         in  tModule mn (nubBy (eqIdent `on` fst) (concat fexps)) (concat texps) sexps (concat cexps) iexps (concat vexps) tds

-- A hack to force evaluation of errors.
-- This should be redone to all happen in the T monad.
tModule :: IdentModule -> [FixDef] -> [TypeExport] -> [SynDef] -> [ClsDef] -> [InstDef] -> [ValueExport] -> [EDef] ->
           TModule [EDef]
tModule mn fs ts ss cs is vs ds =
--  trace ("tmodule " ++ showIdent mn ++ ": " ++ show ts) $
  seqL ts `seq` seqL vs `seq` TModule mn fs ts ss cs is vs ds
  where
    seqL :: forall a . [a] -> ()
    seqL [] = ()
    seqL (x:xs) = x `seq` seqL xs

filterImports :: forall a . (ImportSpec, TModule a) -> (ImportSpec, TModule a)
filterImports it@(ImportSpec _ _ _ Nothing, _) = it
filterImports (imp@(ImportSpec _ _ _ (Just (hide, is))), TModule mn fx ts ss cs ins vs a) =
  let
    keep x xs = elemBy eqIdent x xs `neBool` hide
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
getTVExps :: forall a . M.Map (TModule a) -> TypeTable -> ValueTable -> AssocTable -> ExportItem ->
           ([TypeExport], [ValueExport])
getTVExps impMap _ _ _ (ExpModule m) =
  case M.lookup m impMap of
    Just (TModule _ _ te _ _ _ ve _) -> (te, ve)
    _ -> expErr m
getTVExps _ tys vals ast (ExpTypeCon i) =
  let
    e = expLookup i tys
    qi = tyQIdent e
    ves = getAssocs vals ast qi
  in ([TypeExport i e ves], [])
getTVExps _ tys _ _ (ExpType i) =
  let
    e = expLookup i tys
  in ([TypeExport i e []], [])
getTVExps _ _ vals _ (ExpValue i) =
    ([], [ValueExport i (expLookup i vals)])

-- Export all fixities and synonyms.
-- The synonyms might be needed, and the fixities are harmless
--getFSExps :: forall a . M.Map (TModule a) -> [([FixDef], [SynDef])]
--getFSExps impMap = [ (fe, se) | TModule _ fe _ se _ _ <- M.elems impMap ]

expLookup :: Ident -> M.Map [Entry] -> Entry
expLookup i m =
  case M.lookup i m of
    Just [e] -> e
    Just _ -> errorMessage (getSLocIdent i) $ "ambiguous export " ++ showIdent i
    Nothing -> expErr i

tyQIdent :: Entry -> Ident
tyQIdent (Entry (EVar qi) _) = qi
tyQIdent _ = error "tyQIdent"

eVarI :: SLoc -> String -> Expr
eVarI loc = EVar . mkIdentSLoc loc

expErr :: forall a . Ident -> a
expErr i = errorMessage (getSLocIdent i) $ "export undefined " ++ showIdent i

getAppCon :: EType -> Ident
getAppCon (EVar i) = i
getAppCon (EApp f _) = getAppCon f
getAppCon _ = error "getAppCon"

getApp :: EType -> (Ident, [EType])
getApp = loop []
  where loop as (EVar i) = (i, as)
        loop as (EApp f a) = loop (a:as) f
        loop _ _ = error "getApp"

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
      case M.lookup (qualIdent mn i) tt of
        Just [e] -> e
        _        -> impossible
    -- Find all value Entry for names associated with a type.
    assoc i = getAssocs vt at (qualIdent mn i)

    -- All top level values possible to export.
    ves = [ ValueExport i (Entry (EVar (qualIdent mn i)) ts) | Sign i ts <- tds ]

    -- All top level types possible to export.
    tes =
      [ TypeExport i (tentry i) (assoc i) | Data    (i, _) _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Newtype (i, _) _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Class _ (i, _) _ <- tds ] ++
      [ TypeExport i (tentry i) []        | Type    (i, _) _ <- tds ]

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
      val qi = case M.lookup qi vt of
                 Just [e] -> e
                 _        -> impossible
  in  map (\ qi -> ValueExport (unQualIdent qi) (val qi)) qis

mkTables :: forall a . [(ImportSpec, TModule a)] -> (FixTable, TypeTable, SynTable, ClassTable, InstTable, ValueTable, AssocTable)
mkTables mdls =
  let
    qns (ImportSpec q _ mas _) mn i =
      let
        m = fromMaybe mn mas
      in  if q then [qualIdent m i] else [i, qualIdent m i]
    allValues :: ValueTable
    allValues =
      let
        syms (is, TModule mn _ tes _ _ _ ves _) =
          [ (v, [e]) | ValueExport i e    <- ves,                        v <- qns is mn i ] ++
          [ (v, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, v <- qns is mn i ]
      in  M.fromListWith (unionBy eqEntry) $ concatMap syms mdls
    allSyns =
      let
        syns (_, TModule _ _ _ ses _ _ _ _) = ses
      in  M.fromList (concatMap syns mdls)
    allTypes :: TypeTable
    allTypes =
      let
        types (is, TModule mn _ tes _ _ _ _ _) = [ (v, [e]) | TypeExport i e _ <- tes, v <- qns is mn i ]
      in M.fromListWith (unionBy eqEntry) $ concatMap types mdls
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
      in  M.fromList $ concatMap clss mdls
    allInsts :: InstTable
    allInsts =
      let
        insts (_, TModule _ _ _ _ _ ies _ _) = ies
      in  M.fromListWith mergeInstInfo $ concatMap insts mdls
  in  (allFixes, allTypes, allSyns, allClasses, allInsts, allValues, allAssocs)

mergeInstInfo :: InstInfo -> InstInfo -> InstInfo
mergeInstInfo (InstInfo m1 l1) (InstInfo m2 l2) =
  let
    m = foldr (uncurry $ M.insertWith mrg) m2 (M.toList m1)
    mrg e1 e2 = if eqExpr e1 e2 then e1 else errorMessage (getSLocExpr e1) $ "Multiple instances: " ++ showSLoc (getSLocExpr e2)
    l = unionBy eqInstDict l1 l2
  in  InstInfo m l

eqEntry :: Entry -> Entry -> Bool
eqEntry x y =
  case x of
    Entry ix _ ->
      case y of
        Entry iy _ -> eqIdent (getIdent ix) (getIdent iy)

getIdent :: Expr -> Ident
getIdent ae =
  case ae of
    EVar i -> i
    ECon c -> conIdent c
    _ -> impossible

-- Approximate equality for dictionaries.
-- The important thing is to avoid exact duplicates in the instance table.
eqInstDict :: InstDict -> InstDict -> Bool
eqInstDict (e, _, _, _) (e', _, _, _) = eqExpr e e'

getInstCon :: InstDict -> Ident
getInstCon (_, _, _, t) = getAppCon t

-- Very partial implementation of Expr equality.
-- It is only used to compare instances, so this suffices.
eqExpr :: Expr -> Expr -> Bool
eqExpr (EVar i) (EVar i') = eqIdent i i'
eqExpr (EApp f a) (EApp f' a') = eqExpr f f' && eqExpr a a'
eqExpr _ _ = False

--------------------------

type Typed a = (a, EType)

data TCState = TC
  IdentModule           -- current module name
  Int                   -- unique number
  FixTable              -- fixities, indexed by QIdent
  TypeTable             -- type symbol table
  SynTable              -- synonyms, indexed by QIdent
  ValueTable            -- value symbol table
  AssocTable            -- values associated with a type, indexed by QIdent
  (IM.IntMap EType)     -- mapping from unique id to type
  TCMode                -- pattern, value, or type
  ClassTable            -- class info, indexed by QIdent
  InstTable             -- instances
  Constraints           -- constraints that have to be solved
  --Xderiving (Show)

data TCMode = TCExpr | TCPat | TCType
  --Xderiving (Show)

typeTable :: TCState -> TypeTable
typeTable (TC _ _ _ tt _ _ _ _ _ _ _ _) = tt

valueTable :: TCState -> ValueTable
valueTable (TC _ _ _ _ _ vt _ _ _ _ _ _) = vt

synTable :: TCState -> SynTable
synTable (TC _ _ _ _ st _ _ _ _ _ _ _) = st

fixTable :: TCState -> FixTable
fixTable (TC _ _ ft _ _ _ _ _ _ _ _ _) = ft

assocTable :: TCState -> AssocTable
assocTable (TC _ _ _ _ _ _ ast _ _ _ _ _) = ast

uvarSubst :: TCState -> IM.IntMap EType
uvarSubst (TC _ _ _ _ _ _ _ sub _ _ _ _) = sub

moduleName :: TCState -> IdentModule
moduleName (TC mn _ _ _ _ _ _ _ _ _ _ _) = mn

classTable :: TCState -> ClassTable
classTable (TC _ _ _ _ _ _ _ _ _ ct _ _) = ct

tcMode :: TCState -> TCMode
tcMode (TC _ _ _ _ _ _ _ _ m _ _ _) = m

instTable :: TCState -> InstTable
instTable (TC _ _ _ _ _ _ _ _ _ _ is _) = is

constraints :: TCState -> Constraints
constraints (TC _ _ _ _ _ _ _ _ _ _ _ e) = e

putValueTable :: ValueTable -> T ()
putValueTable venv = T.do
  TC mn n fx tenv senv _ ast sub m cs is es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = T.do
  TC mn n fx _ senv venv ast sub m cs is es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putSynTable :: SynTable -> T ()
putSynTable senv = T.do
  TC mn n fx tenv _ venv ast sub m cs is es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putUvarSubst :: IM.IntMap EType -> T ()
putUvarSubst sub = T.do
  TC mn n fx tenv senv venv ast _ m cs is es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putTCMode :: TCMode -> T ()
putTCMode m = T.do
  TC mn n fx tenv senv venv ast sub _ cs is es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putInstTable :: InstTable -> T ()
putInstTable is = T.do
  TC mn n fx tenv senv venv ast sub m cs _ es <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

putConstraints :: Constraints -> T ()
putConstraints es = T.do
  TC mn n fx tenv senv venv ast sub m cs is _ <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es)

withTCMode :: forall a . TCMode -> T a -> T a
withTCMode m ta = T.do
  om <- gets tcMode
  putTCMode m
  a <- ta
  putTCMode om
  T.return a

-- Use the type table as the value table, and the primKind table as the type table.
withTypeTable :: forall a . T a -> T a
withTypeTable ta = T.do
  TC mn n fx tt st vt ast sub m cs is es <- get
  put (TC mn n fx primKindTable M.empty tt ast sub m cs is es)
  a <- ta
  TC mnr nr _ _ _ ttr astr subr mr csr isr esr <- get
  put (TC mnr nr fx ttr st vt astr subr mr csr isr esr)
  T.return a

addAssocTable :: Ident -> [Ident] -> T ()
addAssocTable i ids = T.do
  TC mn n fx tt st vt ast sub m cs is es <- get
  put $ TC mn n fx tt st vt (M.insert i ids ast) sub m cs is es

addClassTable :: Ident -> ([IdKind], [EConstraint], [Ident]) -> T ()
addClassTable i x = T.do
  TC mn n fx tt st vt ast sub m cs is es <- get
  put $ TC mn n fx tt st vt ast sub m (M.insert i x cs) is es

addInstTable :: [InstDict] -> T ()
addInstTable ics = T.do
  is <- gets instTable
  let mkInstInfo :: InstDict -> InstInfo
      mkInstInfo (e, [], [], EApp _ (EVar i)) = InstInfo (M.singleton i e) []
      mkInstInfo ic = InstInfo M.empty [ic]
  putInstTable $ foldr (\ ic -> M.insertWith mergeInstInfo (getInstCon ic) (mkInstInfo ic)) is ics

addConstraint :: String -> (Ident, EConstraint) -> T ()
addConstraint _msg e@(_d, _ctx) = T.do
--  traceM $ "addConstraint: " ++ msg ++ " " ++ showIdent d ++ " :: " ++ showEType ctx
  TC mn n fx tt st vt ast sub m cs is es <- get
  put $ TC mn n fx tt st vt ast sub m cs is (e : es)

withDict :: forall a . Ident -> EConstraint -> T a -> T a
withDict i c ta = T.do
  is <- gets instTable
  ics <- expandDict (EVar i) c
  addInstTable ics
  a <- ta
  putInstTable is
  T.return a

initTC :: IdentModule -> FixTable -> TypeTable -> SynTable -> ClassTable -> InstTable -> ValueTable -> AssocTable -> TCState
initTC mn fs ts ss cs is vs as =
--  trace ("initTC " ++ show (ts, vs)) $
  let
    xts = foldr (uncurry M.insert) ts primTypes
    xvs = foldr (uncurry M.insert) vs primValues
  in TC mn 1 fs xts ss xvs as IM.empty TCExpr cs is []

kTypeS :: EType
kTypeS = kType

kTypeTypeS :: EType
kTypeTypeS = kArrow kType kType

kTypeTypeTypeS :: EType
kTypeTypeTypeS = kArrow kType $ kArrow kType kType

-- (=>) :: Constraint -> Type -> Type
kConstraintTypeTypeS :: EType
kConstraintTypeTypeS = kArrow kConstraint $ kArrow kType kType

-- (~) :: Type -> Type -> Constraint
kTypeTypeConstraintS :: EType
kTypeTypeConstraintS = kArrow kType (kArrow kType kConstraint)

builtinLoc :: SLoc
builtinLoc = SLoc "builtin" 0 0

mkIdentB :: String -> Ident
mkIdentB = mkIdentSLoc builtinLoc

primKindTable :: KindTable
primKindTable =
  let
    entry i = Entry (EVar (mkIdentB i))
  in M.fromList [
       -- The kinds are wired in (for now)
       (mkIdentB "Primitives.Type",       [entry "Primitives.Type" kTypeS]),
       (mkIdentB "Type",                  [entry "Primitives.Type" kTypeS]),
       (mkIdentB "Constraint",            [entry "Primitives.Constraint" kTypeS]),
       (mkIdentB "Primitives.Constraint", [entry "Primitives.Constraint" kTypeS]),
       (mkIdentB "Primitives.->",         [entry "Primitives.->"   kTypeTypeTypeS]),
       (mkIdentB "->",                    [entry "Primitives.->"   kTypeTypeTypeS])
       ]

primTypes :: [(Ident, [Entry])]
primTypes =
  let
    entry i = Entry (EVar (mkIdentB i))
    k = mkIdent "k"
    kv = EVar k
    kk = IdKind k kTypeS
    tuple n =
      let
        i = tupleConstr builtinLoc n
      in  (i, [entry (unIdent i) $ EForall [kk] $ foldr kArrow kv (replicate n kv)])
  in
      [
       -- The function arrow et al are bothersome to define in Primitives, so keep them here.
       -- But the fixity is defined in Primitives.
       (mkIdentB "->",           [entry "Primitives.->"       kTypeTypeTypeS]),
       (mkIdentB "=>",           [entry "Primitives.=>"       kConstraintTypeTypeS]),
       (mkIdentB "~",            [entry "Primitives.~"        kTypeTypeConstraintS]),
       -- Primitives.hs uses the type [], and it's annoying to fix that.
       (mkIdentB "Data.List.[]", [entry "Data.List.[]"        kTypeTypeS])
      ] ++
      map tuple (enumFromTo 2 10)

primValues :: [(Ident, [Entry])]
primValues =
  let
    tuple n =
      let
        c = tupleConstr builtinLoc n
        vks = [IdKind (mkIdent ("a" ++ showInt i)) kType | i <- enumFromTo 1 n]
        ts = map tVarK vks
        r = tApps c ts
      in  (c, [Entry (ECon $ ConData [(c, n)] c) $ EForall vks $ foldr tArrow r ts ])
  in  map tuple (enumFromTo 2 10)

type T a = TC TCState a

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

{-
isArrow :: EType -> Bool
isArrow = isJust . getArrow
-}

getArrow :: EType -> Maybe (EType, EType)
getArrow (EApp (EApp (EVar n) a) b) =
  if eqIdent n (mkIdent "->") || eqIdent n (mkIdent "Primitives.->") then Just (a, b) else Nothing
getArrow _ = Nothing

getImplies :: EType -> Maybe (EType, EType)
getImplies (EApp (EApp (EVar n) a) b) =
  if eqIdent n (mkIdent "=>") || eqIdent n (mkIdent "Primitives.=>") then Just (a, b) else Nothing
getImplies _ = Nothing

{-
getTuple :: Int -> EType -> Maybe [EType]
getTuple n t = loop t []
  where loop (EVar i) r | isTupleConstr n i && length r == n = Just (reverse r)
        loop (EApp f a) r = loop f (a:r)
        loop _ _ = Nothing
-}

setUVar :: TRef -> EType -> T ()
setUVar i t = T.do
  TC mn n fx tenv senv venv ast sub m cs is es <- get
  put (TC mn n fx tenv senv venv ast (IM.insert i t sub) m cs is es)

getUVar :: Int -> T (Maybe EType)
getUVar i = gets (IM.lookup i . uvarSubst)

munify :: --XHasCallStack =>
          SLoc -> Expected -> EType -> T ()
munify _   (Infer r) b = tSetRefType r b
munify loc (Check a) b = unify loc a b

expandSyn :: --XHasCallStack =>
             EType -> T EType
expandSyn at =
  let
    syn ts t =
      case t of
        EApp f a -> T.do
          aa <- expandSyn a
          syn (aa:ts) f
        EVar i -> T.do
          syns <- gets synTable
          case M.lookup i syns of
            Nothing -> T.return $ foldl tApp t ts
            Just (EForall vks tt) ->
              if length vks /= length ts then tcError (getSLocIdent i) $ "bad synonym use"
                                                                         --X ++ "\nXX " ++ show (i, vks, ts)
              else expandSyn $ subst (zip (map idKindIdent vks) ts) tt
            Just _ -> impossible
        EUVar _ -> T.return $ foldl tApp t ts
        ESign a _ -> expandSyn a   -- Throw away signatures, they don't affect unification
        EForall iks tt | null ts -> EForall iks <$> expandSyn tt
        _ -> impossible
  in syn [] at

derefUVar :: EType -> T EType
derefUVar at =
  case at of
    EApp f a -> T.do
      fx <- derefUVar f
      ax <- derefUVar a
      T.return $ EApp fx ax
    EUVar i -> T.do
      mt <- getUVar i
      case mt of
        Nothing -> T.return at
        Just t -> T.do
          t' <- derefUVar t
          setUVar i t'
          T.return t'
    EVar _ -> T.return at
    ESign t k -> flip ESign k <$> derefUVar t
    EForall iks t -> EForall iks <$> derefUVar t
    _ -> impossible

tcErrorTK :: --XHasCallStack =>
             SLoc -> String -> T ()
tcErrorTK loc msg = T.do
  tcm <- gets tcMode
  let s = case tcm of
            TCType -> "kind"
            _      -> "type"
  tcError loc $ s ++ " error: " ++ msg

unify :: --XHasCallStack =>
         SLoc -> EType -> EType -> T ()
unify loc a b = T.do
  aa <- expandSyn a
  bb <- expandSyn b
  unifyR loc aa bb

-- XXX should do occur check
unifyR :: --XHasCallStack =>
          SLoc -> EType -> EType -> T ()
unifyR _   (EVar x1)    (EVar x2)  | eqIdent x1 x2 = T.return ()
unifyR loc (EApp f1 a1) (EApp f2 a2)               = T.do { unifyR loc f1 f2; unifyR loc a1 a2 }
unifyR _   (EUVar r1)   (EUVar r2) | r1 == r2      = T.return ()
unifyR loc (EUVar r1)   t2                         = unifyVar loc r1 t2
unifyR loc t1           (EUVar r2)                 = unifyVar loc r2 t1
unifyR loc t1           t2                         =
  tcErrorTK loc $ "cannot unify " ++ showExpr t1 ++ " and " ++ showExpr t2

unifyVar :: --XHasCallStack =>
            SLoc -> TRef -> EType -> T ()
unifyVar loc r t = T.do
  mt <- getUVar r
  case mt of
    Nothing -> unifyUnboundVar loc r t
    Just t' -> unify loc t' t

unifyUnboundVar :: --XHasCallStack =>
            SLoc -> TRef -> EType -> T ()
unifyUnboundVar loc r1 at2@(EUVar r2) = T.do
  -- We know r1 /= r2
  mt2 <- getUVar r2
  case mt2 of
    Nothing -> setUVar r1 at2
    Just t2 -> unify loc (EUVar r1) t2
unifyUnboundVar loc r1 t2 = T.do
  vs <- getMetaTyVars [t2]
  if elemBy (==) r1 vs then
    tcErrorTK loc $ "cyclic " ++ showExpr (EUVar r1) ++ " = " ++ showExpr t2
   else
    setUVar r1 t2

-- Reset type variable and unification map
tcReset :: T ()
tcReset = T.do
  TC mn u fx tenv senv venv ast _ m cs is es <- get
  put (TC mn u fx tenv senv venv ast IM.empty m cs is es)

newUVar :: T EType
newUVar = EUVar <$> newUniq

type TRef = Int

newUniq :: T TRef
newUniq = T.do
  TC mn n fx tenv senv venv ast sub m cs is es <- get
  let n' = n+1
  put (seq n' $ TC mn n' fx tenv senv venv ast sub m cs is es)
  T.return n

newIdent :: SLoc -> String -> T Ident
newIdent loc s = T.do
  u <- newUniq
  T.return $ mkIdentSLoc loc $ s ++ "$" ++ showInt u

tLookup :: --XHasCallStack =>
           String -> String -> Ident -> T (Expr, EType)
tLookup msg0 msgN i = T.do
  env <- gets valueTable
  case M.lookup i env of
    Nothing -> tcError (getSLocIdent i) $ msg0 ++ ": " ++ showIdent i
               -- ++ "\n" ++ show (map (unIdent . fst) (M.toList env))
    Just [Entry e s] -> T.return (setSLocExpr (getSLocIdent i) e, s)
    Just _ -> tcError (getSLocIdent i) $ msgN ++ ": " ++ showIdent i

tLookupV :: --XHasCallStack =>
           Ident -> T (Expr, EType)
tLookupV i = T.do
  tcm <- gets tcMode
  let s = case tcm of
            TCType -> "type"
            _      -> "value"
  tLookup ("undefined " ++ s ++ " identifier") ("ambiguous " ++ s ++ " identifier") i

tInst :: (Expr, EType) -> T (Expr, EType)
tInst t = tInst' t T.>>= tDict

tInst' :: (Expr, EType) -> T (Expr, EType)
tInst' et@(ae, at) =
  case at of
    EForall vks t ->
      if null vks then
        T.return (ae, t)
      else T.do
        let vs = map idKindIdent vks
        us <- T.mapM (const newUVar) vks
--        tInst' (ae, subst (zip vs us) t)
        T.return (ae, subst (zip vs us) t)
    _ -> T.return et

tDict :: (Expr, EType) -> T (Expr, EType)
tDict (ae, EApp (EApp (EVar (Ident _ "Primitives.=>")) ctx) t) = T.do
  u <- newUniq
  let d = mkIdentSLoc loc ("dict$" ++ showInt u)
      loc = getSLocExpr ae
  --traceM $ "addConstraint: " ++ showIdent d ++ " :: " ++ showEType ctx ++ " " ++ showSLoc loc
  ctx' <- expandSyn ctx
  addConstraint "from tDict " (d, ctx')
  tDict (EApp ae (EVar d), t)
tDict at = T.return at

extValE :: --XHasCallStack =>
           Ident -> EType -> Expr -> T ()
extValE i t e = T.do
  venv <- gets valueTable
  putValueTable (M.insert i [Entry e t] venv)

-- Extend the symbol table with i = e :: t
-- Add both qualified and unqualified versions of i.
extValETop :: --XHasCallStack =>
              Ident -> EType -> Expr -> T ()
extValETop i t e = T.do
  mn <- gets moduleName
  extValE (qualIdent mn i) t e
  extValE               i  t e

-- Extend symbol table with i::t.
-- The translation for i will be the qualified name.
-- Add both qualified and unqualified versions of i.
extValQTop :: --XHasCallStack =>
              Ident -> EType -> T ()
extValQTop i t = T.do
  mn <- gets moduleName
  extValETop i t (EVar (qualIdent mn i))

extVal :: --XHasCallStack =>
          Ident -> EType -> T ()
extVal i t = extValE i t $ EVar i

extVals :: --XHasCallStack =>
           [(Ident, EType)] -> T ()
extVals = T.mapM_ (uncurry extVal)

extTyp :: Ident -> EType -> T ()
extTyp i t = T.do
  tenv <- gets typeTable
  putTypeTable (M.insert i [Entry (EVar i) t] tenv)

extTyps :: [(Ident, EType)] -> T ()
extTyps = T.mapM_ (uncurry extTyp)

extSyn :: Ident -> EType -> T ()
extSyn i t = T.do
  senv <- gets synTable
  putSynTable (M.insert i t senv)

extFix :: Ident -> Fixity -> T ()
extFix i fx = T.do
  TC mn n fenv tenv senv venv ast sub m cs is es <- get
  put $ TC mn n (M.insert i fx fenv) tenv senv venv ast sub m cs is es
  T.return ()

withExtVal :: forall a . --XHasCallStack =>
              Ident -> EType -> T a -> T a
withExtVal i t ta = T.do
  venv <- gets valueTable
  extVal i t
  a <- ta
  putValueTable venv
  T.return a

withExtVals :: forall a . --XHasCallStack =>
               [(Ident, EType)] -> T a -> T a
withExtVals env ta = T.do
  venv <- gets valueTable
  extVals env
  a <- ta
  putValueTable venv
  T.return a

withExtTyps :: forall a . [IdKind] -> T a -> T a
withExtTyps iks ta = T.do
  let env = map (\ (IdKind v k) -> (v, k)) iks
  venv <- gets typeTable
  extTyps env
  a <- ta
  putTypeTable venv
  T.return a

tcDefs :: [EDef] -> T [EDef]
tcDefs ds = T.do
  T.mapM_ tcAddInfix ds
  dst <- tcDefsType ds
--  traceM (showEDefs dst)
  T.mapM_ addTypeSyn dst
  tcDefsValue dst

tcAddInfix :: EDef -> T ()
tcAddInfix (Infix fx is) = T.do
  mn <- gets moduleName
  T.mapM_ (\ i -> extFix (qualIdent mn i) fx) is
tcAddInfix _ = T.return ()

tcDefsType :: [EDef] -> T [EDef]
tcDefsType ds = withTypeTable $ T.do
  dsk <- T.mapM tcDefKind ds                     -- Check&rename kinds in all type definitions
  T.mapM_ addTypeKind dsk                        -- Add the kind of each type to the environment
  dst <- T.mapM tcDefType dsk                    -- Kind check all type expressions (except local signatures)
  dsc <- T.mapM expandClass dst                  -- Expand all class definitions
  dsi <- T.mapM expandInst (concat dsc)          -- Expand all instance definitions
  T.return (concat dsi)

-- Make sure that the kind expressions are well formed.
tcDefKind :: EDef -> T EDef
tcDefKind adef = T.do
  tcReset
  case adef of
    Data    (i, vks) cs  -> withVks vks kType $ \ vvks _  -> T.return $ Data    (i, vvks) cs
    Newtype (i, vks) c   -> withVks vks kType $ \ vvks _  -> T.return $ Newtype (i, vvks) c
    Type    (i, vks) at  ->
      case at of
        ESign t k        -> withVks vks k     $ \ vvks kr -> T.return $ Type    (i, vvks) (ESign t kr)
        _                -> withVks vks kType $ \ vvks _  -> T.return $ Type    (i, vvks) at
    Class ctx (i, vks) ms-> withVks vks kConstraint $ \ vvks _ -> T.return $ Class ctx (i, vvks) ms
    Instance vks ctx t d -> withVks vks kConstraint $ \ vvks _ -> T.return $ Instance vvks ctx t d
    _                    -> T.return adef

-- Check&rename the given kinds, apply reconstruction at the end
withVks :: forall a . [IdKind] -> EKind -> ([IdKind] -> EKind -> T a) -> T a
withVks vks kr fun = T.do
  (nvks, nkr) <-
    withTypeTable $ T.do
      let
        loop r [] = T.do
          kkr <- tInferTypeT kr
          T.return (reverse r, kkr)
        loop r (IdKind i k : iks) = T.do
          kk <- tInferTypeT k
          withExtVal i kk $ loop (IdKind i kk : r) iks
      loop [] vks
  fun nvks nkr

-- Add symbol table entries (with kind) for all top level typeish definitions
addTypeKind :: EDef -> T ()
addTypeKind adef = T.do
  let
    addAssoc i is = T.do
      mn <- gets moduleName
      addAssocTable (qualIdent mn i) (map (qualIdent mn) is)
    assocData (Constr c (Left _)) = [c]
    assocData (Constr c (Right its)) = c : map fst its
  case adef of
    Data    lhs@(i, _) cs -> T.do
      addLHSKind lhs kType
      addAssoc i (nubBy eqIdent $ concatMap assocData cs)
    Newtype lhs@(i, _) c  -> T.do
      addLHSKind lhs kType
      addAssoc i (assocData c)
    Type    lhs t         -> addLHSKind lhs (getTypeKind t)
    Class _ lhs@(i, _) ms -> T.do
      addLHSKind lhs kConstraint
      addAssoc i (mkClassConstructor i : [ m | BSign m _ <- ms ])
    _               -> T.return ()

getTypeKind :: EType -> EKind
getTypeKind (ESign _ k) = k
getTypeKind _ = kType

addLHSKind :: LHS -> EKind -> T ()
addLHSKind (i, vks) kret =
--  trace ("addLHSKind " ++ showIdent i ++ " :: " ++ showExpr (lhsKind vks kret)) $
  extValQTop i (lhsKind vks kret)

lhsKind :: [IdKind] -> EKind -> EKind
lhsKind vks kret = foldr (\ (IdKind _ k) -> kArrow k) kret vks

-- Add type synonyms to the value table
addTypeSyn :: EDef -> T ()
addTypeSyn adef =
  case adef of
    Type (i, vs) t -> T.do
      extSyn i (EForall vs t)
      mn <- gets moduleName
      extSyn (qualIdent mn i) (EForall vs t)
    _ -> T.return ()

-- Do kind checking of all typeish definitions.
tcDefType :: EDef -> T EDef
tcDefType d = T.do
  tcReset
  case d of
    Data    lhs@(_, iks) cs     -> withVars iks $ Data    lhs   <$> T.mapM tcConstr cs
    Newtype lhs@(_, iks) c      -> withVars iks $ Newtype lhs   <$> tcConstr c
    Type    lhs@(_, iks)    t   -> withVars iks $ Type    lhs   <$> tInferTypeT t
    Sign         i          t   ->                Sign    i     <$> tCheckTypeT kType t
    ForImp  ie i            t   ->                ForImp ie i   <$> tCheckTypeT kType t
    Class   ctx lhs@(_, iks) ms -> withVars iks $ Class         <$> tcCtx ctx T.<*> T.return lhs              T.<*> T.mapM tcMethod ms
    Instance iks ctx c m        -> withVars iks $ Instance iks  <$> tcCtx ctx T.<*> tCheckTypeT kConstraint c T.<*> T.return m
    _                           -> T.return d
 where
   tcCtx = T.mapM (tCheckTypeT kConstraint)
   tcMethod (BSign i t) = BSign i <$> tcTypeT (Check kType) t
   tcMethod m = T.return m

withVars :: forall a . [IdKind] -> T a -> T a
withVars aiks ta =
  case aiks of
    [] -> ta
    IdKind i k : iks -> T.do
      withExtVal i k $ withVars iks ta

tcConstr :: Constr -> T Constr
tcConstr (Constr c ets) =
  Constr c <$> either (\ x -> Left  T.<$> T.mapM (\ t     ->          tcTypeT (Check kType) t) x)
                      (\ x -> Right T.<$> T.mapM (\ (i,t) -> (i,) <$> tcTypeT (Check kType) t) x) ets


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
expandClass :: EDef -> T [EDef]
expandClass dcls@(Class ctx (iCls, vks) ms) = T.do
  mn <- gets moduleName
  let
      methIds = [ i | (BSign i _) <- ms ]
      meths = [ b | b@(BSign _ _) <- ms ]
      mdflts = [ (i, eqns) | BFcn i eqns <- ms ]
      tCtx = tApps (qualIdent mn iCls) (map (EVar . idKindIdent) vks)
      mkDflt (BSign methId t) = [ Sign iDflt $ EForall vks $ tCtx `tImplies` t, def $ lookupBy eqIdent methId mdflts ]
        where def Nothing = Fcn iDflt [Eqn [] $ EAlts [([], noDflt)] []]
              def (Just eqns) = Fcn iDflt eqns
              iDflt = mkDefaultMethodId methId
              -- XXX This isn't right, "Prelude._nodefault" might not be in scope
              noDflt = EApp noDefaultE (ELit noSLoc (LStr (unIdent iCls ++ "." ++ unIdent methId)))
      mkDflt _ = impossible
      dDflts = concatMap mkDflt meths
  addClassTable (qualIdent mn iCls) (vks, ctx, methIds)
  T.return $ dcls : dDflts
expandClass d = T.return [d]

noDefaultE :: Expr
noDefaultE = ELit noSLoc $ LPrim "noDefault"

-- Turn (unqualified) class and method names into a default method name
mkDefaultMethodId :: Ident -> Ident
mkDefaultMethodId meth = addIdentSuffix meth "$dflt"

{-
clsToDict :: EType -> T EType
clsToDict = T.do
  -- XXX for now, only allow contexts of the form (C t1 ... tn)
  let usup as (EVar c) | isConIdent c = T.return (tApps c as)
      usup as (EApp f a) = usup (a:as) f
      usup _ t = tcError (getSLocExpr t) ("bad context " ++ showEType t)
  usup []
-}

addConstraints :: [EConstraint] -> EType -> EType
addConstraints []  t = t
addConstraints cs  t = tupleConstraints cs `tImplies` t

tupleConstraints :: [EConstraint] -> EConstraint
tupleConstraints []  = error "tupleConstraints"
tupleConstraints [c] = c
tupleConstraints cs  = tApps (tupleConstr noSLoc (length cs)) cs

expandInst :: EDef -> T [EDef]
expandInst dinst@(Instance vks ctx cc bs) = T.do
  let loc = getSLocExpr cc
      iCls = getAppCon cc
  iInst <- newIdent loc "inst"
  let sign = Sign iInst (eForall vks $ addConstraints ctx cc)
  (e, _) <- tLookupV iCls
  ct <- gets classTable
  let qiCls = getAppCon e
  (_, supers, mis) <-
    case M.lookup qiCls ct of
      Nothing -> tcError loc $ "not a class " ++ showIdent qiCls
      Just x -> T.return x
  let (bs', (_, ims)) =
          let f (BFcn i eqns) = S.do
                (n, xs) <- S.get
                let mi = mkIdentSLoc (getSLocIdent i) ("meth$" ++ showInt n)
                S.put (n+1, (i, mi):xs)
                S.return (BFcn mi eqns)
              f b = S.return b
          in  S.runState (S.mapM f bs) (1, [])
      meths = map meth mis
        where meth i = EVar $ fromMaybe (mkDefaultMethodId i) $ lookupBy eqIdent i ims
      sups = map (const (EVar $ mkIdentSLoc loc "dict$")) supers
      args = sups ++ meths
  let bind = Fcn iInst [Eqn [] $ EAlts [([], foldl EApp (EVar $ mkClassConstructor iCls) args)] bs']
  mn <- gets moduleName
  addInstTable [(EVar $ qualIdent mn iInst, vks, ctx, cc)]
  T.return [dinst, sign, bind]
expandInst d = T.return [d]

eForall :: [IdKind] -> EType -> EType
eForall [] t = t
eForall vs t = EForall vs t

---------------------

tcDefsValue :: [EDef] -> T [EDef]
tcDefsValue ds = T.do
  T.mapM_ addValueType ds
  T.mapM (\ d -> T.do { tcReset; tcDefValue d}) ds

addValueType :: EDef -> T ()
addValueType adef = T.do
  mn <- gets moduleName
  case adef of
    Sign i t -> extValQTop i t
    Data (i, vks) cs -> T.do
      let
        cti = [ (qualIdent mn c, either length length ets) | Constr c ets <- cs ]
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
        addCon (Constr c ets) = T.do
          let ts = either id (map snd) ets
          extValETop c (EForall vks $ foldr tArrow tret ts) (ECon $ ConData cti (qualIdent mn c))
      T.mapM_ addCon cs
    Newtype (i, vks) (Constr c fs) -> T.do
      let
        t = head $ either id (map snd) fs
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
      extValETop c (EForall vks $ tArrow t tret) (ECon $ ConNew (qualIdent mn c))
    ForImp _ i t -> extValQTop i t
    Class ctx (i, vks) ms -> addValueClass ctx i vks ms
    _ -> T.return ()

addValueClass :: [EConstraint] -> Ident -> [IdKind] -> [EBind] -> T ()
addValueClass ctx iCls vks ms = T.do
  mn <- gets moduleName
  let
      meths = [ b | b@(BSign _ _) <- ms ]
      methTys = map (\ (BSign _ t) -> t) meths
      supTys = ctx  -- XXX should do some checking
      targs = supTys ++ methTys
      qiCls = qualIdent mn iCls
      tret = tApps qiCls (map tVarK vks)
      cti = [ (qualIdent mn iCon, length targs) ]
      iCon = mkClassConstructor iCls
  extValETop iCon (EForall vks $ foldr tArrow tret targs) (ECon $ ConData cti (qualIdent mn iCon))
  let addMethod (BSign i t) = extValETop i (EForall vks $ tApps qiCls (map (EVar . idKindIdent) vks) `tImplies` t) (EVar $ qualIdent mn i)
      addMethod _ = impossible
--  traceM ("addValueClass " ++ showEType (ETuple ctx))
  T.mapM_ addMethod meths

{-
bundleConstraints :: [EConstraint] -> EType -> EType
bundleConstraints []  t = t
bundleConstraints [c] t = tImplies c t
bundleConstraints cs  t = tImplies (ETuple cs) t
-}

mkClassConstructor :: Ident -> Ident
mkClassConstructor i = addIdentSuffix i "$C"

unForall :: EType -> ([IdKind], EType)
unForall (EForall iks t) = (iks, t)
unForall t = ([], t)

tcDefValue :: --XHasCallStack =>
              EDef -> T EDef
tcDefValue adef =
  case adef of
    Fcn i eqns -> T.do
      (_, tt) <- tLookup "no type signature" "many type signatures" i
      let (iks, tfn) = unForall tt
--      traceM $ "tcDefValue: " ++ showIdent i ++ " :: " ++ showExpr tt
      mn <- gets moduleName
      teqns <- withExtTyps iks $ tcEqns tfn eqns
--      traceM (showEDefs [Fcn i eqns, Fcn i teqns])
      checkConstraints
      T.return $ Fcn (qualIdent mn i) teqns
    ForImp ie i t -> T.do
      mn <- gets moduleName
      T.return (ForImp ie (qualIdent mn i) t)
    _ -> T.return adef

tCheckTypeT :: EType -> EType -> T EType
tCheckTypeT = tCheck tcTypeT

tInferTypeT :: EType -> T EType
tInferTypeT t = fst <$> tInfer tcTypeT t

-- Kind check a type while already in type checking mode
tcTypeT :: --XHasCallStack =>
           Expected -> EType -> T EType
tcTypeT mk t = withTCMode TCType (tcExpr mk (dsType t))

-- Kind check a type while in value checking mode
tcType :: --XHasCallStack =>
          Expected -> EType -> T EType
tcType mk = withTypeTable . tcTypeT mk

{-
-- Sort check a kind while already in type cheking mode
tcKind :: --XHasCallStack =>
          EKind -> T EKind
tcKind e = fst <$> withTypeTable (tcType (Just kType) e)
-}

-- When inferring the type, the resulting type will
-- be assigned to the TRef (using tSetRefType),
-- and can then be read of (using tGetRefType).
-- When checking, the expected type is simple given.
data Expected = Infer TRef | Check EType
  --Xderiving(Show)

tInfer :: forall a . --XHasCallStack =>
          (Expected -> a -> T a) -> a -> T (Typed a)
tInfer tc a = T.do
  ref <- newUniq
  a' <- tc (Infer ref) a
  t <- tGetRefType ref
  T.return (a', t)

tCheck :: forall a . (Expected -> a -> T a) -> EType -> a -> T a
tCheck tc t = tc (Check t)

tInferExpr :: --XHasCallStack =>
              Expr -> T (Typed Expr)
tInferExpr = tInfer tcExpr

tCheckExpr :: --XHasCallStack =>
              EType -> Expr -> T Expr
tCheckExpr t e | Just (ctx, t') <- getImplies t = T.do
  _ <- undefined -- XXX
  u <- newUniq
  let d = mkIdentSLoc (getSLocExpr e) ("adict$" ++ showInt u)
  e' <- withDict d ctx $ tCheckExpr t' e
  T.return $ ELam [EVar d] e'
tCheckExpr t e = tCheck tcExpr t e

tGetRefType :: --XHasCallStack =>
               TRef -> T EType
tGetRefType ref = T.do
  m <- gets uvarSubst
  case IM.lookup ref m of
    Nothing -> error "tGetRefType"
    Just t -> T.return t

-- Set the type for an Infer
tSetRefType :: --XHasCallStack =>
               TRef -> EType -> T ()
tSetRefType ref t = T.do
  m <- gets uvarSubst
  case IM.lookup ref m of
    Nothing -> putUvarSubst (IM.insert ref t m)
    Just _ -> error "tSetRefType"

-- Get the type of an already set Expected
tGetExpType :: Expected -> T EType
tGetExpType (Check t) = T.return t
tGetExpType (Infer r) = tGetRefType r

-- Get the type of an unset Expected
tGetExpTypeSet :: Expected -> T EType
tGetExpTypeSet (Check t) = T.return t
tGetExpTypeSet (Infer r) = T.do
  t <- newUVar
  tSetRefType r t
  T.return t

tcExpr :: --XHasCallStack =>
          Expected -> Expr -> T Expr
tcExpr mt ae = T.do
--  traceM ("tcExpr enter: " ++ showExpr ae ++ " :: " ++ showMaybe showExpr mt)
  r <- tcExprR mt ae
--  t <- expandType (snd r)
--  traceM ("tcExpr exit: " ++ showExpr (fst r) ++ " :: " ++ showExpr t)
  T.return r
tcExprR :: --XHasCallStack =>
           Expected -> Expr -> T Expr
tcExprR mt ae =
  let { loc = getSLocExpr ae } in
  case ae of
    EVar i -> T.do
      tcm <- gets tcMode
      case tcm of
        TCPat | isDummyIdent i -> T.do
                -- _ can be anything, so just ignore it
                _ <- tGetExpTypeSet mt
                T.return ae

              | isConIdent i -> T.do
                ipt <- tLookupV i
                (p, pt) <- tInst' ipt  -- XXX
                -- We will only have an expected type for a non-nullary constructor
                case mt of
                  Check ext -> subsCheck loc p ext pt
                  Infer r   -> T.do { tSetRefType r pt; T.return p }

              | otherwise -> T.do
                -- All pattern variables are in the environment as
                -- type references.  Assign the reference the given type.
                ext <- tGetExpTypeSet mt
                (p, t) <- tLookupV i
                case t of
                  EUVar r -> tSetRefType r ext
                  _ -> impossible
                T.return p
          
        _ | eqIdent i (mkIdent "dict$") -> T.do
          -- Magic variable that just becomes the dictionary
          d <- newIdent (getSLocIdent i) "dict$"
          case mt of
            Infer _ -> impossible
            Check t -> T.do
              t' <- expandSyn t
              addConstraint "from dict$" (d, t')
          T.return (EVar d)

        _ -> T.do
          -- Type checking an expression (or type)
          T.when (isDummyIdent i) impossible
          (e, t) <- tLookupV i
          -- Variables bound in patterns start with an (EUVar ref) type,
          -- which can be instantiated to a polytype.
          -- Dereference such a ref.
          t' <-
            case t of
              EUVar r -> T.fmap (fromMaybe t) (getUVar r)
              _ -> T.return t
--          traceM ("EVar " ++ showIdent i ++ " :: " ++ showExpr t ++ " = " ++ showExpr t')
          instSigma loc e t' mt

    EApp f a -> T.do
      (f', ft) <- tInferExpr f
      (at, rt) <- unArrow loc ft
      tcm <- gets tcMode
--      traceM ("tcExpr EApp: " ++ showExpr f ++ " :: " ++ showEType ft)
      case tcm of
        TCPat -> T.do
          a' <- tCheckExpr at a
          instPatSigma loc rt mt
          T.return (EApp f' a')
        _ -> T.do
          a' <- checkSigma a at
          instSigma loc (EApp f' a') rt mt

    EOper e ies -> T.do e' <- tcOper e ies; tcExpr mt e'
    ELam ps e -> tcExprLam mt ps e
    ELit loc' l -> tcLit mt loc' l
    ECase a arms -> T.do
      (ea, ta) <- tInferExpr a
      tt <- tGetExpTypeSet mt
      earms <- T.mapM (tcArm tt ta) arms
      T.return (ECase ea earms)
    ELet bs a -> tcBinds bs $ \ ebs -> T.do { ea <- tcExpr mt a; T.return (ELet ebs ea) }
    ETuple es -> T.do
      let
        n = length es
      (ees, tes) <- T.fmap unzip (T.mapM tInferExpr es)
      let
        ttup = tApps (tupleConstr loc n) tes
      munify loc mt ttup
      T.return (ETuple ees)
    EDo mmn ass -> T.do
      case ass of
        [] -> impossible
        [as] ->
          case as of
            SThen a -> tcExpr mt a
            _ -> tcError loc $ "bad final do statement"
        as : ss -> T.do
          case as of
            SBind p a -> T.do
              let
                sbind = maybe (mkIdentSLoc loc ">>=") (\ mn -> qualIdent mn (mkIdentSLoc loc ">>=")) mmn
              tcExpr mt (EApp (EApp (EVar sbind) a)
                              (ELam [eVarI loc "$x"] (ECase (eVarI loc "$x") [(p, EAlts [([], EDo mmn ss)] [])])))
            SThen a -> T.do
              let
                sthen = maybe (mkIdentSLoc loc ">>") (\ mn -> qualIdent mn (mkIdentSLoc loc ">>") ) mmn
              tcExpr mt (EApp (EApp (EVar sthen) a) (EDo mmn ss))
                
            SLet bs ->
              tcExpr mt (ELet bs (EDo mmn ss))

    ESectL e i -> tcExpr mt (EApp (EVar i) e)
    ESectR i e -> T.do
      let x = eVarI loc "$x"
      tcExpr mt (ELam [x] (EApp (EApp (EVar i) x) e))
    EIf e1 e2 e3 -> T.do
      e1' <- tCheckExpr (tBool (getSLocExpr e1)) e1
      case mt of
        Check t -> T.do
          e2' <- checkSigma e2 t
          e3' <- checkSigma e3 t
          T.return (EIf e1' e2' e3')
        Infer ref -> T.do
          (e2', t2) <- tInferExpr e2
          (e3', t3) <- tInferExpr e3
          e2'' <- subsCheck loc e2' t2 t3
          e3'' <- subsCheck loc e3' t3 t2
          tSetRefType ref t2
          T.return (EIf e1' e2'' e3'')

    EListish (LList es) -> T.do
      te <- newUVar
      munify loc mt (tApp (tList loc) te)
      es' <- T.mapM (tCheckExpr te) es
      T.return (EListish (LList es'))
    EListish (LCompr eret ass) -> T.do
      let
        doStmts :: [EStmt] -> [EStmt] -> T ([EStmt], Typed Expr)
        doStmts rss xs =
          case xs of
            [] -> T.do
              r <- tInferExpr eret
              T.return (reverse rss, r)
            as : ss ->
              case as of
                SBind p a -> T.do
                  v <- newUVar
                  ea <- tCheckExpr (tApp (tList loc) v) a
                  tCheckPat v p $ \ ep -> doStmts (SBind ep ea : rss) ss
                SThen a -> T.do
                  ea <- tCheckExpr (tBool (getSLocExpr a)) a
                  doStmts (SThen ea : rss) ss
                SLet bs ->
                  tcBinds bs $ \ ebs ->
                    doStmts (SLet ebs : rss) ss
      (rss, (ea, ta)) <- doStmts [] ass
      let
        tr = tApp (tList loc) ta
      munify loc mt tr
      T.return (EListish (LCompr ea rss))
    EListish (LFrom       e)        -> tcExpr mt (enum loc "From" [e])
    EListish (LFromTo     e1 e2)    -> tcExpr mt (enum loc "FromTo" [e1, e2])
    EListish (LFromThen   e1 e2)    -> tcExpr mt (enum loc "FromThen" [e1,e2])
    EListish (LFromThenTo e1 e2 e3) -> tcExpr mt (enum loc "FromThenTo" [e1,e2,e3])
    ESign e t -> T.do
      t' <- tcType (Check kType) t
      tcm <- gets tcMode
      case tcm of
        TCPat -> T.do
          instPatSigma loc t' mt
          tCheckExpr t' e
        _ -> T.do
          e' <- instSigma loc e t' mt
          checkSigma e' t'
    EAt i e -> T.do
      (_, ti) <- tLookupV i
      e' <- tcExpr mt e
      tt <- tGetExpType mt
      case ti of
        EUVar r -> tSetRefType r tt
        _ -> impossible
      T.return (EAt i e')
    EForall vks t ->
      withVks vks kType $ \ vvks _ -> T.do
        tt <- withVars vvks (tcExpr mt t)
        T.return (EForall vvks tt)
    _ -> impossible

enum :: SLoc -> String -> [Expr] -> Expr
enum loc f = foldl EApp (EVar (mkIdentSLoc loc ("enum" ++ f)))

tcLit :: Expected -> SLoc -> Lit -> T Expr
tcLit mt loc l = T.do
  let lit t = instSigma loc (ELit loc l) t mt
  case l of
    LInt _  -> lit (tConI loc "Primitives.Int")
    LDouble _ -> lit (tConI loc "Primitives.Double")
    LChar _ -> lit (tConI loc "Primitives.Char")
    LStr _  -> lit (tApp (tConI loc "Data.List.[]") (tConI loc "Primitives.Char"))
    LPrim _ -> newUVar T.>>= lit  -- pretend it is anything
    LForImp _ -> impossible

tcOper :: --XHasCallStack =>
          Expr -> [(Ident, Expr)] -> T Expr
tcOper ae aies = T.do
  let
    doOp (e1:e2:es) o os ies =
      let e = EApp (EApp o e2) e1
      in  calc (e:es) os ies
    doOp _ _ _ _ = impossible

    calc :: [Expr] -> [(Expr, Fixity)] -> [((Expr, Fixity), Expr)] -> Expr
    calc [et] [] [] = et
    calc es ((o, _):os) [] = doOp es o os []
    calc es oos@((oy, (ay, py)):os) iies@((oo@(ox, (ax, px)), e) : ies) =
--      traceM (show ((unIdent (getIdent (fst o)), ay, py), (unIdent i, ax, px)))
      if px == py && (not (eqAssoc ax ay) || eqAssoc ax AssocNone) then
        errorMessage (getSLocExpr ox) "ambiguous operator expression"
       else if px < py || eqAssoc ax AssocLeft && px == py then
        doOp es oy os iies
       else
        calc (e:es) (oo : oos) ies
    calc es [] ((o, e) : ies) =
      calc (e:es) [o] ies
    calc _ _ _ = impossible

    opfix :: FixTable -> (Ident, Expr) -> T ((Expr, Fixity), Expr)
    opfix fixs (i, e) = T.do
      (ei, _) <- tLookupV i
      let fx = getFixity fixs (getIdent ei)
      T.return ((EVar i, fx), e)

  fixs <- gets fixTable
--  traceM $ unlines $ map show [(unIdent i, fx) | (i, fx) <- M.toList fixs]
  ites <- T.mapM (opfix fixs) aies
  T.return $ calc [ae] [] ites

unArrow :: --XHasCallStack =>
           SLoc -> EType -> T (EType, EType)
unArrow loc t = T.do
  case getArrow t of
    Just ar -> T.return ar
    Nothing -> T.do
      a <- newUVar
      r <- newUVar
      unify loc t (tArrow a r)
      T.return (a, r)

getFixity :: FixTable -> Ident -> Fixity
getFixity fixs i = fromMaybe (AssocLeft, 9) $ M.lookup i fixs

tcPats :: forall a . EType -> [EPat] -> (EType -> [EPat] -> T a) -> T a
tcPats t [] ta = ta t []
tcPats t (p:ps) ta = T.do
  (tp, tr) <- unArrow (getSLocExpr p) t
  tCheckPat tp p $ \ pp -> tcPats tr ps $ \ tt pps -> ta tt (pp : pps)

tcExprLam :: Expected -> [EPat] -> Expr -> T Expr
tcExprLam mt aps expr = T.do
  t <- tGetExpTypeSet mt
  tcPats t aps $ \ tt ps -> T.do
    er <- tCheckExpr tt expr
    T.return (ELam ps er)

tcEqns :: EType -> [Eqn] -> T [Eqn]
--tcEqns t eqns | trace ("tcEqns: " ++ showEBind (BFcn dummyIdent eqns) ++ " :: " ++ showEType t) False = undefined
tcEqns t eqns | Just (ctx, t') <- getImplies t = T.do
  let loc = getSLocEqns eqns
  d <- newIdent loc "adict"
  f <- newIdent loc "fcnD"
  withDict d ctx $ T.do
    eqns' <- tcEqns t' eqns
    let eqn =
          case eqns' of
            [Eqn [] alts] -> Eqn [EVar d] alts
            _             -> Eqn [EVar d] $ EAlts [([], EVar f)] [BFcn f eqns']
    T.return [eqn]
tcEqns t eqns = T.do
  let loc = getSLocEqns eqns
  f <- newIdent loc "fcnS"
  (eqns', ds) <- solveLocalConstraints $ T.mapM (tcEqn t) eqns
  case ds of
    [] -> T.return eqns'
    _  -> T.do
      let
        bs = eBinds ds
        eqn = Eqn [] $ EAlts [([], EVar f)] (bs ++ [BFcn f eqns'])
      T.return [eqn]

tcEqn :: EType -> Eqn -> T Eqn
tcEqn t eqn =
  case eqn of
    Eqn ps alts -> tcPats t ps $ \ tt ps' -> T.do
      aalts <- tcAlts tt alts
      T.return (Eqn ps' aalts)

tcAlts :: EType -> EAlts -> T EAlts
tcAlts tt (EAlts alts bs) =
  tcBinds bs $ \ bbs -> T.do { aalts <- T.mapM (tcAlt tt) alts; T.return (EAlts aalts bbs) }

tcAlt :: EType -> EAlt -> T EAlt
--tcAlt t _ | trace ("tcAlt: " ++ showExpr t) False = undefined
tcAlt t (ss, rhs) = tcGuards ss $ \ sss -> T.do { rrhs <- tCheckExpr t rhs; T.return (sss, rrhs) }

tcGuards :: forall a . [EStmt] -> ([EStmt] -> T a) -> T a
tcGuards [] ta = ta []
tcGuards (s:ss) ta = tcGuard s $ \ rs -> tcGuards ss $ \ rss -> ta (rs:rss)

tcGuard :: forall a . EStmt -> (EStmt -> T a) -> T a
tcGuard (SBind p e) ta = T.do
  (ee, tt) <- tInferExpr e
  tCheckPat tt p $ \ pp -> ta (SBind pp ee)
tcGuard (SThen e) ta = T.do
  ee <- tCheckExpr (tBool (getSLocExpr e)) e
  ta (SThen ee)
tcGuard (SLet bs) ta = tcBinds bs $ \ bbs -> ta (SLet bbs)

tcArm :: EType -> EType -> ECaseArm -> T ECaseArm
tcArm t tpat arm =
  case arm of
    (p, alts) -> tCheckPat tpat p $ \ pp -> T.do
      aalts <- tcAlts t alts
      T.return (pp, aalts)

eBinds :: [(Ident, Expr)] -> [EBind]
eBinds ds = [BFcn i [Eqn [] (EAlts [([], e)] [])] | (i, e) <- ds]

instPatSigma :: --XHasCallStack =>
                 SLoc -> Sigma -> Expected -> T ()
instPatSigma _   pt (Infer r) = tSetRefType r pt
instPatSigma loc pt (Check t) = T.do { _ <- subsCheck loc undefined t pt; T.return () } -- XXX really?

subsCheck :: --XHasCallStack =>
              SLoc -> Expr -> Sigma -> Sigma -> T Expr
-- (subsCheck args off exp) checks that
-- 'off' is at least as polymorphic as 'args -> exp'
subsCheck loc exp1 sigma1 sigma2 = T.do -- Rule DEEP-SKOL
  (skol_tvs, rho2) <- skolemise sigma2
  exp1' <- subsCheckRho loc exp1 sigma1 rho2
  esc_tvs <- getFreeTyVars [sigma1,sigma2]
  let bad_tvs = filter (\ i -> elemBy eqIdent i esc_tvs) skol_tvs
  T.when (not (null bad_tvs)) $
    tcErrorTK loc "Subsumption check failed"
  T.return exp1'

tCheckPat :: forall a . EType -> EPat -> (EPat -> T a) -> T a
tCheckPat t p@(EVar v) ta | not (isConIdent v) = T.do  -- simple special case
  withExtVals [(v, t)] $ ta p
tCheckPat t ap ta = T.do
--  traceM $ "tcPat: " ++ show ap
  let vs = patVars ap
  multCheck vs
  env <- T.mapM (\ v -> (v,) <$> newUVar) vs
  withExtVals env $ T.do
    pp <- withTCMode TCPat $ tCheckExpr t ap
    () <- checkArity 0 pp
    ta pp

multCheck :: [Ident] -> T ()
multCheck vs =
  T.when (anySameBy eqIdent vs) $ T.do
    let v = head vs
    tcError (getSLocIdent v) $ "Multiply defined: " ++ showIdent v

checkArity :: Int -> EPat -> T ()
checkArity n (EApp f a) = T.do
  checkArity (n+1) f
  checkArity 0 a
checkArity n (ECon c) =
  let a = conArity c
  in  if n < a then
        tcError (getSLocCon c) "too few arguments"
      else if n > a then
        tcError (getSLocCon c) "too many arguments"
      else
        T.return ()
checkArity n (EAt _ p) = checkArity n p
checkArity n (ESign p _) = checkArity n p
checkArity n p =
  case p of
    ETuple _           -> check0
    EListish (LList _) -> check0
    EVar _             -> check0
    ELit _ _           -> check0
    _ ->
         --Xerror (show p)
         impossible
  where
    check0 = if n /= 0 then tcError (getSLocExpr p) "Bad pattern" else T.return ()

tcBinds :: forall a . [EBind] -> ([EBind] -> T a) -> T a
tcBinds xbs ta = T.do
  let
    tmap = M.fromList [ (i, t) | BSign i t <- xbs ]
    xs = concatMap getBindVars xbs
  multCheck xs
  xts <- T.mapM (tcBindVarT tmap) xs
  withExtVals xts $ T.do
    nbs <- T.mapM tcBind xbs
    ta nbs

tcBindVarT :: M.Map EType -> Ident -> T (Ident, EType)
tcBindVarT tmap x = T.do
  case M.lookup x tmap of
    Nothing -> T.do
      t <- newUVar
      T.return (x, t)
    Just t -> T.do
      tt <- withTypeTable $ tcTypeT (Check kType) t
      T.return (x, tt)

tcBind :: EBind -> T EBind
tcBind abind =
  case abind of
    BFcn i eqns -> T.do
      (_, tt) <- tLookupV i
      let (iks, tfn) = unForall tt
      teqns <- withExtTyps iks $ tcEqns tfn eqns
      T.return $ BFcn i teqns
    BPat p a -> T.do
      (ep, tp) <- withTCMode TCPat $ tInferExpr p  -- pattern variables already bound
      ea       <- tCheckExpr tp a
      T.return $ BPat ep ea
    BSign _ _ -> T.return abind

getBindVars :: EBind -> [Ident]
getBindVars abind =
  case abind of
    BFcn i _  -> [i]
    BPat p _  -> patVars p
    BSign _ _ -> []

-- Desugar [T] and (T,T,...)
dsType :: EType -> EType
dsType at =
  case at of
    EVar _ -> at
    EApp f a -> EApp (dsType f) (dsType a)
    EOper t ies -> EOper (dsType t) [(i, dsType e) | (i, e) <- ies]
    EListish (LList [t]) -> tApp (tList (getSLocExpr at)) (dsType t)
    ETuple ts -> tApps (tupleConstr (getSLocExpr at) (length ts)) (map dsType ts)
    ESign t k -> ESign (dsType t) k
    EForall iks t -> EForall iks (dsType t)
    _ -> impossible

tConI :: SLoc -> String -> EType
tConI loc = tCon . mkIdentSLoc loc

tListI :: SLoc -> Ident
tListI loc = mkIdentSLoc loc "Data.List.[]"

tList :: SLoc -> EType
tList = tCon . tListI

tBool :: SLoc -> EType
tBool loc = tConI loc "Data.Bool_Type.Bool"

impossible :: --XHasCallStack =>
              forall a . a
impossible = error "impossible"

showTModule :: forall a . (a -> String) -> TModule a -> String
showTModule sh amdl =
  case amdl of
    TModule mn _ _ _ _ _ _ a -> "Tmodule " ++ showIdent mn ++ "\n" ++ sh a ++ "\n"

{-
showValueTable :: ValueTable -> String
showValueTable vt =
  unlines $ take 5 [showIdent i ++ " : " ++ showExpr t | (i, [Entry _ t]) <- M.toList vt]
-}

-----------------------------------------------------

getFreeTyVars :: [EType] -> T [TyVar]
getFreeTyVars tys = T.do
  tys' <- T.mapM derefUVar tys
  T.return (freeTyVars tys')

getMetaTyVars :: [EType] -> T [TRef]
getMetaTyVars tys = T.do
  tys' <- T.mapM derefUVar tys
  T.return (metaTvs tys')

getEnvTypes :: T [EType]
getEnvTypes = gets (map entryType . concat . M.elems . valueTable)

{-
quantify :: [MetaTv] -> Rho -> T Sigma
-- Quantify over the specified type variables (all flexible)
quantify tvs ty = T.do
   T.mapM_ bind (tvs `zip` new_bndrs) -- 'bind' is just a cunning way
   ty' <- zonkType ty               -- of doing the substitution
   T.return (EForall new_bndrs_kind ty')
  where
    used_bndrs = tyVarBndrs ty -- Avoid quantified type variables in use
    new_bndrs = deleteFirstsBy eqIdent allBinders used_bndrs
    bind (tv, name) = writeTcRef tv (EVar name)
    new_bndrs_kind = map (\ i -> IdKind i undefined) new_bndrs

allBinders :: [Ident] -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders = [ mkIdent [chr x] | x <- [ord 'a' .. ord 'z'] ] ++
             [ mkIdent (chr x : showInt i) | i <- [1 ..], x <- [ord 'a' .. ord 'z']]
-}

skolemise :: --XHasCallStack =>
             Sigma -> T ([TyVar], Rho)
-- Performs deep skolemisation, returning the
-- skolem constants and the skolemised type
skolemise (EForall tvs ty) = T.do -- Rule PRPOLY
  sks1 <- T.mapM (newSkolemTyVar . idKindIdent) tvs
  (sks2, ty') <- skolemise (subst (zip (map idKindIdent tvs) (map EVar sks1)) ty)
  T.return (sks1 ++ sks2, ty')
skolemise t@(EApp _ _) | Just (arg_ty, res_ty) <- getArrow t = T.do -- Rule PRFUN
  (sks, res_ty') <- skolemise res_ty
  T.return (sks, arg_ty `tArrow` res_ty')
skolemise (EApp f a) = T.do
  (sks1, f') <- skolemise f
  (sks2, a') <- skolemise a
  T.return (sks1 ++ sks2, EApp f' a')
skolemise ty =
  T.return ([], ty) -- Rule PRMONO

-- Skolem tyvars are just identifiers that start with a uniq
newSkolemTyVar :: Ident -> T Ident
newSkolemTyVar tv = T.do
  uniq <- newUniq
  T.return (mkIdentSLoc (getSLocIdent tv) (showInt uniq ++ unIdent tv))

freeTyVars :: [EType] -> [TyVar]
-- Get the free TyVars from a type; no duplicates in result
freeTyVars = foldr (go []) []
  where
    go :: [TyVar] -- Ignore occurrences of bound type variables
       -> EType   -- Type to look at
       -> [TyVar] -- Accumulates result
       -> [TyVar]
    go bound (EVar tv) acc
      | elemBy eqIdent tv bound = acc
      | elemBy eqIdent tv acc = acc
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
    go _ _ = undefined

{-
tyVarBndrs :: Rho -> [TyVar]
-- Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs ty = nubBy eqIdent (bndrs ty)
  where
    bndrs (EForall tvs body) = map idKindIdent tvs ++ bndrs body
    bndrs (EApp arg res) = bndrs arg ++ bndrs res
    bndrs (EVar _) = []
    bndrs _ = undefined

inferSigma :: Expr -> T (Expr, Sigma)
inferSigma e = T.do
  (e', exp_ty) <- inferRho e
  env_tys      <- getEnvTypes
  env_tvs      <- getMetaTyVars env_tys
  res_tvs      <- getMetaTyVars [exp_ty]
  let forall_tvs = deleteFirstsBy eqInt res_tvs env_tvs
  (e',) <$> quantify forall_tvs exp_ty
-}

checkSigma :: Expr -> Sigma -> T Expr
checkSigma expr sigma = T.do
  (skol_tvs, rho) <- skolemise sigma
  expr' <- tCheckExpr rho expr
  if null skol_tvs then
    -- Fast special case
    T.return expr'
   else T.do
    env_tys <- getEnvTypes
    esc_tvs <- getFreeTyVars (sigma : env_tys)
    let bad_tvs = filter (\ i -> elemBy eqIdent i esc_tvs) skol_tvs
    T.when (not (null bad_tvs)) $
      tcErrorTK (getSLocExpr expr) "not polymorphic enough"
    T.return expr'

subsCheckRho :: --XHasCallStack =>
                SLoc -> Expr -> Sigma -> Rho -> T Expr
--subsCheckRho _ e1 t1 t2 | trace ("subsCheckRho: " ++ {-showExpr e1 ++ " :: " ++ -} showEType t1 ++ " = " ++ showEType t2) False = undefined
subsCheckRho loc exp1 sigma1@(EForall _ _) rho2 = T.do -- Rule SPEC
  (exp1', rho1) <- tInst (exp1, sigma1)
  subsCheckRho loc exp1' rho1 rho2
subsCheckRho loc exp1 rho1 rho2 | Just (a2, r2) <- getArrow rho2 = T.do -- Rule FUN
  (a1, r1) <- unArrow loc rho1
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 rho1 rho2 | Just (a1, r1) <- getArrow rho1 = T.do -- Rule FUN
  (a2,r2) <- unArrow loc rho2
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 tau1 tau2 = T.do  -- Rule MONO
  unify loc tau1 tau2 -- Revert to ordinary unification
  T.return exp1

subsCheckFun :: SLoc -> Expr -> Sigma -> Rho -> Sigma -> Rho -> T Expr
subsCheckFun loc e1 a1 r1 a2 r2 = T.do
  _ <- subsCheck loc undefined a2 a1   -- XXX
  subsCheckRho loc e1 r1 r2

instSigma :: --XHasCallStack =>
             SLoc -> Expr -> Sigma -> Expected -> T Expr
instSigma loc e1 t1 (Check t2) = subsCheckRho loc e1 t1 t2
instSigma _   e1 t1 (Infer r) = T.do
  (e1', t1') <- tInst (e1, t1)
  tSetRefType r t1'
  T.return e1'

-----

-- Given a dictionary of a (constraint type), split it up
--  * name components of a tupled constraint
--  * name superclasses of a constraint
expandDict :: Expr -> EConstraint -> T [InstDict]
expandDict edict acn = T.do
  cn <- expandSyn acn
  let
    (iCls, args) = getApp cn
  case getTupleConstr iCls of
    Just _ -> concat <$> T.mapM (\ (i, a) -> expandDict (mkTupleSel i (length args) `EApp` edict) a) (zip [0..] args)
    Nothing -> T.do
      ct <- gets classTable
      let (iks, sups, _) = fromMaybe impossible $ M.lookup iCls ct
          sub = zip (map idKindIdent iks) args
          sups' = map (subst sub) sups
      mn <- gets moduleName
      insts <- concat <$> T.mapM (\ (i, sup) -> expandDict (EVar (mkSuperSel mn iCls i) `EApp` edict) sup) (zip [1 ..] sups')
      T.return $ (edict, [], [], cn) : insts

mkSuperSel :: IdentModule -> Ident -> Int -> Ident
mkSuperSel mn c i = qualIdent mn $ mkIdent $ unIdent c ++ "$super" ++ showInt i

---------------------------------

-- Solve constraints generated locally in 'ta'.
-- Keep any unsolved ones for later.
solveLocalConstraints :: forall a . T a -> T (a, [(Ident, Expr)])
solveLocalConstraints ta = T.do
  cs <- gets constraints           -- old constraints
  putConstraints []                -- start empty
  a <- ta                          -- compute, generating constraints
  ds <- solveConstraints           -- solve those
  un <- gets constraints           -- get remaining unsolved
  putConstraints (un ++ cs)        -- put back unsolved and old constraints
  T.return (a, ds)

{-
showInstDict :: InstDict -> String
showInstDict (e, iks, ctx, ct) = showExpr e ++ " :: " ++ showEType (eForall iks $ addConstraints ctx ct)

showInstDef :: InstDef -> String
showInstDef (cls, InstInfo m ds) = "instDef " ++ showIdent cls ++ ": "
            ++ showList (showPair showIdent showExpr) (M.toList m) ++ ", " ++ showList showInstDict ds

showConstraint :: (Ident, EConstraint) -> String
showConstraint (i, t) = showIdent i ++ " :: " ++ showEType t

showMatch :: (Expr, [EConstraint]) -> String
showMatch (e, ts) = showExpr e ++ " " ++ showList showEType ts
-}

-- Solve as many constraints as possible.
-- Return bindings for the dictionary witnesses.
-- Unimplemented:
--  instances with a context
solveConstraints :: T [(Ident, Expr)]
solveConstraints = T.do
  cs <- gets constraints
  if null cs then
    T.return []
   else T.do
--    traceM "------------------------------------------\nsolveConstraints"
    cs' <- T.mapM (\ (i,t) -> T.do { t' <- derefUVar t; T.return (i,t') }) cs
--    traceM ("constraints:\n" ++ unlines (map showConstraint cs'))
    it <- gets instTable
--    traceM ("instances:\n" ++ unlines (map showInstDef (M.toList it)))
    let solve :: [(Ident, EType)] -> [(Ident, EType)] -> [(Ident, Expr)] -> T ([(Ident, EType)], [(Ident, Expr)])
        solve [] uns sol = T.return (uns, sol)
        solve (cns@(di, ct) : cnss) uns sol = T.do
--          traceM ("trying " ++ showEType ct)
          let loc = getSLocIdent di
              (iCls, cts) = getApp ct
          case getTupleConstr iCls of
            Just _ -> T.do
              goals <- T.mapM (\ c -> T.do { d <- newIdent loc "dict"; T.return (d, c) }) cts
--              traceM ("split tuple " ++ showList showConstraint goals)
              solve (goals ++ cnss) uns ((di, ETuple (map (EVar . fst) goals)) : sol)
            Nothing ->
              case M.lookup iCls it of
                Nothing -> T.do
--                  traceM ("class missing " ++ showIdent iCls)
                  solve cnss (cns : uns) sol   -- no instances, so no chance
                Just (InstInfo atomMap insts) ->
                  case cts of
                    [EVar i] -> T.do
--                      traceM ("solveSimple " ++ showIdent i ++ " -> " ++ showMaybe showExpr (M.lookup i atomMap))
                      solveSimple (M.lookup i atomMap) cns cnss uns sol
                    _        -> solveGen loc insts cns cnss uns sol

        -- An instance of the form (C T)
        solveSimple Nothing  cns     cnss uns sol = solve cnss (cns : uns)            sol   -- no instance
        solveSimple (Just e) (di, _) cnss uns sol = solve cnss        uns  ((di, e) : sol)  -- e is the dictionary expression

        solveGen loc insts cns@(di, ct) cnss uns sol = T.do
--          traceM ("solveGen " ++ showEType ct)
          let matches = getBestMatches $ findMatches insts ct
--          traceM ("matches " ++ showList showMatch matches)
          case matches of
            []          -> solve cnss (cns : uns) sol
            [(de, ctx)] ->
              if null ctx then
                solve cnss uns ((di, de) : sol)
              else T.do
                d <- newIdent loc "dict"
--                traceM ("constraint " ++ showIdent di ++ " :: " ++ showEType ct ++ "\n" ++
--                        "   turns into " ++ showIdent d ++ " :: " ++ showEType (tupleConstraints ctx) ++ ", " ++
--                        showIdent di ++ " = " ++ showExpr (EApp de (EVar d)))
                solve ((d, tupleConstraints ctx) : cnss) uns ((di, EApp de (EVar d)) : sol)
            _           -> tcError loc $ "Multiple constraint solutions for: " ++ showEType ct

    (unsolved, solved) <- solve cs' [] []
    putConstraints unsolved
--    traceM ("solved:\n"   ++ unlines [ showIdent i ++ " = "  ++ showExpr  e | (i, e) <- solved ])
--    traceM ("unsolved:\n" ++ unlines [ showIdent i ++ " :: " ++ showEType t | (i, t) <- unsolved ])
    T.return solved

-- Given some instances and a constraint, find the matching instances.
-- For each matching instance return: (subst-size, (dict-expression, new-constraints))
-- The subst-size is the size of the substitution that made the input instance match.
-- It is a measure of how exact the match is.
findMatches :: [InstDict] -> EConstraint -> [(Int, (Expr, [EConstraint]))]
findMatches ds ct =
 let rrr =
       [ (length s, (de, map (substEUVar s . subst r) ctx))
       | (de, iks, ctx, t) <- ds, let { r = freshSubst iks }, Just s <- [matchType [] (subst r t) ct] ]
 in --trace ("findMatches: " ++ showList showInstDict ds ++ "; " ++ showEType ct ++ "; " ++ show rrr)
    rrr
  where

    -- Change type variable to unique unification variables.
    -- These unification variables will never leak out of findMatches.
    freshSubst iks = zipWith (\ ik j -> (idKindIdent ik, EUVar j)) iks [1000000000 ..] -- make sure the variables are unique

    -- Match two types, instantiate variables in the first type.
    matchType r (EVar i) (EVar i') | eqIdent i i' = Just r
    matchType r (EApp f a) (EApp f' a') = -- XXX should use Maybe monad
      case matchType r f f' of
        Nothing -> Nothing
        Just r' -> matchType r' a a'
    matchType r (EUVar i) t =
      -- For a variable, check that any previous match is the same.
      case lookup i r of
        Just t' -> if eqEType t t' then Just r else Nothing
        Nothing -> Just ((i, t) : r)
    matchType _ _ _ = Nothing

    -- Do substitution for EUVar.
    -- XXX similat to derefUVar
    substEUVar [] t = t
    substEUVar _ t@(EVar _) = t
    substEUVar s (EApp f a) = EApp (substEUVar s f) (substEUVar s a)
    substEUVar s t@(EUVar i) = fromMaybe t $ lookup i s
    substEUVar s (EForall iks t) = EForall iks (substEUVar s t)
    substEUVar _ _ = impossible


-- Get the best matches.  These are the matches with the smallest substitution.
getBestMatches :: [(Int, (Expr, [EConstraint]))] -> [(Expr, [EConstraint])]
getBestMatches [] = []
getBestMatches ms =
  let b = minimum (map fst ms)         -- minimum substitution size
  in  [ ec | (s, ec) <- ms, s == b ]   -- pick out the smallest

-- Check that there are no unsolved constraints.
checkConstraints :: T ()
checkConstraints = T.do
  cs <- gets constraints
  case cs of
    [] -> T.return ()
    (i, t) : _ -> T.do
      t' <- derefUVar t
      tcError (getSLocIdent i) $ "Cannot satisfy constraint: " ++ showExpr t'
      --traceM $ "Cannot satisfy constraint: " ++ showExpr t'
      --T.return ()
