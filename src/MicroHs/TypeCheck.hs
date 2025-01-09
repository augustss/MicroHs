-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}
module MicroHs.TypeCheck(
  typeCheck,
  TModule(..), showTModule,
  GlobTables, emptyGlobTables, mergeGlobTables,
  impossible, impossibleShow,
  mkClassConstructor,
  mkSuperSel,
  setBindings,
  boolPrefix,
  listPrefix,
  ValueExport(..), TypeExport(..),
  Symbols,
  ) where
import Prelude(); import MHSPrelude
import Control.Applicative
import Control.Arrow(first)
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import MicroHs.Builtin
import MicroHs.Deriving
import MicroHs.Expr
import MicroHs.Fixity
import MicroHs.Graph
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import qualified MicroHs.IntMap as IM
import MicroHs.List
import MicroHs.MRnf
import MicroHs.Parse(dotDotIdent)
import MicroHs.SymTab
import MicroHs.TCMonad
import GHC.Stack
import Debug.Trace

boolPrefix :: String
boolPrefix = "Data.Bool_Type."

listPrefix :: String
listPrefix = "Data.List_Type."

nameList :: String
nameList = listPrefix ++ "[]"
identList :: Ident
identList = mkIdentB nameList

nameInt :: String
nameInt = "Primitives.Int"
identInt :: Ident
identInt = mkIdentB nameInt

nameWord :: String
nameWord = "Primitives.Word"
identWord :: Ident
identWord = mkIdentB nameWord

nameFloatW :: String
nameFloatW = "Primitives.FloatW"
identFloatW :: Ident
identFloatW = mkIdentB nameFloatW

nameChar :: String
nameChar = "Primitives.Char"
identChar :: Ident
identChar = mkIdentB nameChar

nameInteger :: String
nameInteger = "Data.Integer_Type.Integer"
identInteger :: Ident
identInteger = mkIdentB nameInteger

nameTypeEq :: String
nameTypeEq = "Primitives.~"
identTypeEq :: Ident
identTypeEq = mkIdentB nameTypeEq

nameImplies :: String
nameImplies = "Primitives.=>"
identImplies :: Ident
identImplies = mkIdentB nameImplies

nameArrow :: String
nameArrow = "Primitives.->"
identArrow :: Ident
identArrow = mkIdentB nameArrow

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

nameCoercible :: String
nameCoercible = "Data.Coerce.Coercible"

--primitiveKinds :: [String]
--primitiveKinds = [nameType, nameConstraint, nameSymbol, nameNat]

----------------------

-- Certain data structures persist during the entire compilation
-- session.  The information is needed beyond the scope where it was defined.
data GlobTables = GlobTables {
  gSynTable   :: SynTable,        -- type synonyms are needed for expansion
  gDataTable  :: DataTable,       -- data/newtype definitions
  gClassTable :: ClassTable,      -- classes are neede for superclass expansion
  gInstInfo   :: InstTable        -- instances are implicitely global
  }

instance MRnf GlobTables where
  mrnf (GlobTables a b c d) = mrnf a `seq` mrnf b `seq` mrnf c `seq` mrnf d

emptyGlobTables :: GlobTables
emptyGlobTables = GlobTables { gSynTable = M.empty, gDataTable = M.empty, gClassTable = M.empty, gInstInfo = M.empty }

mergeGlobTables :: GlobTables -> GlobTables -> GlobTables
mergeGlobTables g1 g2 =
  GlobTables { gSynTable = M.merge (gSynTable g1) (gSynTable g2),
               gDataTable = M.merge (gDataTable g1) (gDataTable g2),
               gClassTable = M.merge (gClassTable g1) (gClassTable g2),
               gInstInfo = M.mergeWith mergeInstInfo (gInstInfo g1) (gInstInfo g2) }

type Symbols = (SymTab, SymTab)

data TModule a = TModule {
  tModuleName :: IdentModule,     -- module names
  tFixDefs    :: [FixDef],        -- all fixities, exported or not
  tTypeExps   :: [TypeExport],    -- exported types
  tValueExps  :: [ValueExport],   -- exported values (including from T(..))
  tDefaults   :: Defaults,        -- exported defaults
  tBindingsOf :: a                -- bindings
  }
--  deriving (Show)

instance MRnf a => MRnf (TModule a) where
  mrnf (TModule a b c d e f) = mrnf a `seq` mrnf b `seq` mrnf c `seq` mrnf d `seq` mrnf e `seq` mrnf f

setBindings :: TModule b -> a -> TModule a
setBindings (TModule x y z w v _) a = TModule x y z w v a

type FixDef = (Ident, Fixity)

type Sigma = EType
type Rho   = EType

typeCheck :: forall a . GlobTables -> ImpType -> [(ImportSpec, TModule a)] -> EModule -> (TModule [EDef], GlobTables, Symbols)
typeCheck globs impt aimps (EModule mn exps defs) =
--  trace (unlines $ map (showTModuleExps . snd) aimps) $
  let
    imps = map filterImports aimps
    tc = mkTCState mn globs imps
  in case tcRun (tcDefs impt defs) tc of
       (tds, tcs) ->
         let
           thisMdl = (mn, mkTModule impt tds tcs)
           impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ _ m mm _, tm) <- imps]
           impMap = M.fromList [(i, m) | (i, m) <- thisMdl : impMdls]
           (texps, vexps) =
             unzip $ map (getTVExps impMap (typeTable tcs) (valueTable tcs) (assocTable tcs)) exps
           fexps = map tFixDefs (M.elems impMap)
           sexps = synTable tcs
           dexps = dataTable tcs
           iexps = instTable tcs
           ctbl  = classTable tcs
           dflts = M.fromList $ filter ((`elem` ds) . fst) $ M.toList $ defaults tcs
                 where ds = [ tyQIdent $ expLookup ti (typeTable tcs) | ExpDefault ti <- exps ]
         in  ( tModule mn (nubBy ((==) `on` fst) (concat fexps)) (concat texps) (concat vexps) dflts tds
             , GlobTables { gSynTable = sexps, gDataTable = dexps, gClassTable = ctbl, gInstInfo = iexps }
             , (typeTable tcs, valueTable tcs)
             )

-- A hack to force evaluation of errors.
-- This should be redone to all happen in the T monad.
tModule :: IdentModule -> [FixDef] -> [TypeExport] -> [ValueExport] -> Defaults -> [EDef] ->
           TModule [EDef]
tModule mn fs ts vs ds bs =
--  trace ("tmodule " ++ showIdent mn ++ ":\n" ++ show vs) $
  tseq ts `seq` vseq vs `seq` ds `seq` TModule mn fs ts vs ds bs
  where
    tseq [] = ()
    tseq (TypeExport _ e _:xs) = e `seq` tseq xs
    vseq [] = ()
    vseq (ValueExport _ e:xs) = e `seq` vseq xs

filterImports :: forall a . (ImportSpec, TModule a) -> (ImportSpec, TModule a)
filterImports it@(ImportSpec _ _ _ _ Nothing, _) = it
filterImports (imp@(ImportSpec _ _ _ _ (Just (hide, is))), TModule mn fx ts vs ds a) =
  let
    keep x xs = elem x xs /= hide
    ivs  = [ i | ImpValue i <- is ]
    vs'  = filter (\ (ValueExport i _) -> keep i ivs) vs ++
           if hide then []
           else [ ve | TypeExport _ _ ves <- ts, ve@(ValueExport i _) <- ves, i `elem` ivs ]
    aits = [ i | ImpTypeAll i <- is ]         -- all T(..) imports
    its  = [ i | ImpTypeSome i _ <- is ] ++ aits
    -- XXX This isn't quite right, hiding T() should hide T, but not the constructors
    ts' =
      if hide then
        let ok xs (ValueExport i _) = i `notElem` ivs && i `notElem` xs in
        [ TypeExport i e (filter (ok []) ves) | TypeExport i e ves <- ts, i `notElem` its ] ++
        [ TypeExport i e (filter (ok xs) ves) | TypeExport i e ves <- ts, ImpTypeSome i' xs <- is, i == i' ]
      else
        let ok xs (ValueExport i _) = i `elem` ivs || i `elem` xs || isDefaultMethodId i in
        [ TypeExport i e                 ves  | TypeExport i e ves <- ts, i `elem` aits ] ++
        [ TypeExport i e (filter (ok xs) ves) | TypeExport i e ves <- ts, ImpTypeSome i' xs <- is, i == i' ]
    msg = "not exported"
    allVs = map (\ (ValueExport i _) -> i) vs ++
            concatMap (\ (TypeExport _ _ xvs) -> map (\ (ValueExport i _) -> i) xvs) ts
    allTs = map (\ (TypeExport i _ _) -> i) ts
  in
    (if hide then
       id -- don't complain about missing hidden identifiers; we use it for compatibility
     else
       checkBad msg (ivs \\ allVs) .
       checkBad msg (its \\ allTs))
    --trace (show (ts, vs)) $
    (imp, TModule mn fx ts' vs' ds a)

checkBad :: forall a . String -> [Ident] -> a -> a
checkBad _ [] a = a
checkBad msg (i:_) _ =
  errorMessage (getSLoc i) $ msg ++ ": " ++ showIdent i

-- Type and value exports
getTVExps :: forall a . M.Map (TModule a) -> TypeTable -> ValueTable -> AssocTable -> ExportItem ->
             ([TypeExport], [ValueExport])
getTVExps impMap _ _ _ (ExpModule m) =
  case M.lookup m impMap of
    Just (TModule _ _ te ve _ _) -> (te, ve)
    _ -> errorMessage (getSLoc m) $ "undefined module: " ++ showIdent m
getTVExps _ tys vals ast (ExpTypeSome ti is) =
  let e = expLookup ti tys
      assc = getAssocs vals ast $ tyQIdent e   -- all associated values
      ves = concatMap one is
      one i | i == dotDotIdent = assc          -- '..' means all assocaited values
            | otherwise =
              case filter (\ (ValueExport i' _) -> i == i') assc of
                ee : _ -> [ee]                 -- Pick the assocaited value if it exists
                [] -> [ValueExport (unQualIdent i) $ expLookup i vals]
                                               -- otherwise, just look up a pattern synonym.
                                               -- This might accidentally pick up a constructor from
                                               -- another type, but it doesn't really matter.
  in ([TypeExport (unQualIdent ti) e ves], [])
  
getTVExps _ _ vals _ (ExpValue i) = ([], [ValueExport (unQualIdent i) (expLookup i vals)])
getTVExps _ _ _ _ (ExpDefault _) = ([], [])

expLookup :: Ident -> SymTab -> Entry
expLookup i m = either (errorMessage (getSLoc i)) id $ stLookup "export" i m

tyQIdent :: Entry -> Ident
tyQIdent (Entry (EVar qi) _) = qi
tyQIdent _ = error "tyQIdent"

eVarI :: SLoc -> String -> Expr
eVarI loc = EVar . mkIdentSLoc loc

getApp :: HasCallStack => EType -> (Ident, [EType])
getApp t = fromMaybe (impossibleShow t) $ getAppM t

-- Construct a dummy TModule for the currently compiled module.
-- It has all the relevant export tables.
-- The value&type export tables will later be filtered through the export list.
mkTModule :: forall a . HasCallStack => ImpType -> [EDef] -> TCState -> TModule a
mkTModule impt tds tcs =
  let
    mn = moduleName tcs
    tt = typeTable  tcs
    at = assocTable tcs
    vt = valueTable tcs

    -- Find the Entry for a type.
    tentry i =
      case stLookup "" (qualIdent mn i) tt of
        Right e -> e
        _       -> impossible
    ventry i t =
      let qi = qualIdent mn i in
      case stLookup "" qi vt of
        Right e -> e
        _       -> Entry (EVar qi) t  -- XXX A hack for boot modules
          
    -- Find all value Entry for names associated with a type.
    assoc i = case impt of
                ImpBoot -> []  -- XXX For boot files the tables are not set up correctly.
                _ -> getAssocs vt at (qualIdent mn i)

    -- All top level values possible to export.
    ves = [ ValueExport i (ventry i t) | Sign is t <- tds, i <- is ]

    -- All top level types possible to export.
    tes =
      [ TypeExport i (tentry i) (assoc i) | Data    (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Newtype (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Class _ (i, _) _ _ <- tds ] ++
      [ TypeExport i (tentry i) []        | Type    (i, _) _   <- tds ]

    -- All fixity declaration.
    fes = [ (qualIdent mn i, fx) | Infix fx is <- tds, i <- is ]

    -- All defaults
    des = defaults tcs

  in  TModule mn fes tes ves des impossible

-- Find all value Entry for names associated with a type.
-- XXX join stLookup code with tentry
getAssocs :: (HasCallStack) => ValueTable -> AssocTable -> Ident -> [ValueExport]
getAssocs _vt at ai = fromMaybe [] $ M.lookup ai at

mkTCState :: IdentModule -> GlobTables -> [(ImportSpec, TModule a)] -> TCState
mkTCState mdlName globs mdls =
  let
    allValues :: ValueTable
    allValues =
      let
        usyms (ImportSpec _ qual _ _ _, TModule _ _ tes ves _ _) =
          if qual then [] else
          [ (i, [e]) | ValueExport i e    <- ves, not (isInstId i)  ] ++
          [ (i, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, not (isDefaultMethodId i) ]
        qsyms (ImportSpec _ _ _ mas _, TModule mn _ tes ves _ _) =
          let m = fromMaybe mn mas in
          [ (v, [e]) | ValueExport i e    <- ves,                        let { v = qualIdent    m i } ] ++
          [ (v, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, let { v = qualIdentD e m i } ] ++
          [ (v, [Entry (EVar v) t]) | (i, ClassInfo _ _ t _ _) <- M.toList (gClassTable globs), let { v = mkClassConstructor i } ]
        -- Default methods are always entered with their qualified original name.
        qualIdentD (Entry e _) m i | not (isDefaultMethodId i) = qualIdent m i
                                   | otherwise = 
                                     case e of
                                       EVar qi -> qi
                                       _ -> undefined
      in  stFromList (concatMap usyms mdls) (concatMap qsyms mdls)
    allTypes :: TypeTable
    allTypes =
      let
        usyms (ImportSpec _ qual _ _ _, TModule _ _ tes _ _ _) =
          if qual then [] else [ (i, [e]) | TypeExport i e _ <- tes ]
        qsyms (ImportSpec _ _ _ mas _, TModule mn _ tes _ _ _) =
          let m = fromMaybe mn mas in
          [ (qualIdent m i, [e]) | TypeExport i e _ <- tes ]
      in stFromList (concatMap usyms mdls) (concatMap qsyms mdls)

    allFixes :: FixTable
    allFixes = M.fromList (concatMap (tFixDefs . snd) mdls)
    allAssocs :: AssocTable
    allAssocs =
      let assocs (_, TModule _ _ tes _ _ _) = [ (tyQIdent e, cs) | TypeExport _ e cs <- tes ]
      in  M.fromList $ concatMap assocs mdls

    dflts = foldr mergeDefaults M.empty (map (tDefaults . snd) mdls)

  in TC { moduleName = mdlName,
          unique = 1,
          fixTable = addPrimFixs allFixes,
          typeTable = foldr (uncurry stInsertGlbA) allTypes primTypes,
          synTable = gSynTable globs,
          dataTable = gDataTable globs,
          valueTable = foldr (uncurry stInsertGlbA) allValues primValues,
          assocTable = allAssocs,
          uvarSubst = IM.empty,
          tcMode = TCExpr,
          classTable = gClassTable globs,
          ctxTables = (gInstInfo globs, [], [], []),
          constraints = [],
          defaults = dflts
        }

mergeDefaults :: Defaults -> Defaults -> Defaults
mergeDefaults ds = foldr (uncurry $ M.insertWith mrg) ds . M.toList
  where mrg :: [EType] -> [EType] -> [EType]
        mrg ts ts' | null (filter (\ t -> not (elemBy eqEType t ts)) ts') = ts
                   | null (filter (\ t -> not (elemBy eqEType t ts')) ts) = ts'
                   | otherwise = []

mergeInstInfo :: InstInfo -> InstInfo -> InstInfo
mergeInstInfo (InstInfo m1 l1 fds) (InstInfo m2 l2 _) =
  let
    m = foldr (uncurry $ M.insertWith mrg) m2 (M.toList m1)
    mrg e1@(EVar i1) (EVar i2)
      | i1 == i2  = e1
      | otherwise = errorMessage (getSLoc i1) $ "Multiple instances: " ++ showSLoc (getSLoc i2)
    mrg e1 _e2 = e1 -- XXX improve this
    l = unionBy eqInstDict l1 l2
  in  InstInfo m l fds

-- Approximate equality for dictionaries.
-- The important thing is to avoid exact duplicates in the instance table.
eqInstDict :: InstDict -> InstDict -> Bool
eqInstDict (e, _) (e', _) = eqExpr e e'

-- Identifier should only be seen with it's qualified name.
isInstId :: Ident -> Bool
isInstId i = (instPrefix ++ uniqIdentSep) `isPrefixOf` unIdent i

mkInstId :: SLoc -> EType -> Ident
mkInstId loc ct = mkIdentSLoc loc $ instPrefix ++ uniqIdentSep ++ clsTy
  where clsTy = map (\ c -> if isSpace c then '@' else c) $ showExprRaw ct

instPrefix :: String
instPrefix = "inst"

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
  
addAssocTable :: Ident -> [ValueExport] -> T ()
addAssocTable i ids = modify $ \ ts -> ts { assocTable = M.insert i ids (assocTable ts) }

addClassTable :: Ident -> ClassInfo -> T ()
addClassTable i x = modify $ \ ts -> ts { classTable = M.insert i x (classTable ts) }

addInstTable :: [InstDictC] -> T ()
addInstTable ics = do
--  tcTrace $ "addInstTable: " ++ show ics
  let
    -- Change type variable to unique unification variables.
    -- These unification variables will never leak, but as an extra caution
    -- we use negative numbers..
    freshSubst u iks =
      zipWith (\ ik j -> (idKindIdent ik, EUVar j)) iks [u ..]

    mkInstInfo :: InstDictC -> T (Ident, InstInfo)
    mkInstInfo (e, iks, ctx, ct, fds) = do
      case (iks, ctx, getApp ct) of
        ([], [], (c, [EVar i])) -> return $ (c, InstInfo (M.singleton i e) [] fds)
        (_,  _,  (c, ts      )) -> return $ (c, InstInfo M.empty [(e, ii)] fds)
          where ii u =
                  let ctx' = map (subst s) ctx
                      ts'  = map (subst s) ts
                      s    = freshSubst u iks
                  in  (ctx', ts')
  iis <- mapM mkInstInfo ics
  it <- gets instTable
  putInstTable $ foldr (uncurry $ M.insertWith mergeInstInfo) it iis

addConstraint :: Ident -> EConstraint -> T ()
addConstraint d ctx = do
--  tcTrace $ "addConstraint: " ++ showIdent d ++ " :: " ++ showEType ctx
  ctx' <- expandSyn ctx
  modify $ \ ts -> ts{ constraints = (d, ctx') : constraints ts }

withDicts :: forall a . HasCallStack => [(Ident, EConstraint)] -> T a -> T a
withDicts ds ta = do
  ct <- gets ctxTables
  mapM_ addDict ds
  a <- ta
  putCtxTables ct
  return a

withDict :: forall a . HasCallStack => Ident -> EConstraint -> T a -> T a
withDict i c ta = do
--  tcTrace $ "+++ withDict enter " ++ show (i, c)
  ct <- gets ctxTables
  addDict (i, c)
  a <- ta
  putCtxTables ct
--  tcTrace $ "--- withDict leave " ++ show (i, c)
  return a

addDict :: HasCallStack => (Ident, EConstraint) -> T ()
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
  addArgDict i c

addEqDict :: Ident -> EType -> EType -> T ()
addEqDict _i t1 t2 = do
  is <- gets typeEqTable
--  tcTrace ("withEqDict: " ++ show (is, (t1,t2), (addTypeEq t1 t2 is)))
  putTypeEqTable (addTypeEq t1 t2 is)

addMetaDict :: HasCallStack => Ident -> EConstraint -> T ()
addMetaDict i c = do
  ms <- gets metaTable
  putMetaTable ((i,c) : ms)

addArgDict :: HasCallStack => Ident -> EConstraint -> T ()
addArgDict i c = do
  ads <- gets argDicts
  putArgDicts ((i,c) : ads)

--stdDefaults :: [EType]
--stdDefaults = [EVar identInteger, EVar identFloatW, EApp (EVar identList) (EVar identChar)]

addPrimFixs :: FixTable -> FixTable
addPrimFixs =
  M.insert (mkIdent "Primitives.->") (AssocRight, -1) .
  M.insert (mkIdent "Primitives.=>") (AssocRight, -2)

-- r for 'realm', suggested by ChatGPT
rSort :: ESort
rSort = EVar (mkIdent "Primitives.Sort")

sKindKindKind :: EKind
sKindKindKind = sArrow sKind (sArrow sKind sKind)

kTypeTypeS :: EType
kTypeTypeS = kArrow kType kType

kTypeTypeTypeS :: EType
kTypeTypeTypeS = kArrow kType $ kArrow kType kType

mkIdentB :: String -> Ident
mkIdentB = mkIdentSLoc builtinLoc

-- E.g.
--  Kind :: Sort
primSortTable :: KindTable
primSortTable =
  let
    entry i s = Entry (EVar (mkIdentB i)) s
    qsorts = [
       -- The kinds are wired in (for now)
       (mkIdentB nameKind,       [entry nameKind rSort])
       ]
  in stFromList (map (first unQualIdent) qsorts) qsorts

-- E.g.
--  Type       :: Kind
--  Constraint :: Kind
--  (->)       :: Kind -> Kind -> Kind
primKindTable :: KindTable
primKindTable =
  let
    entry i k = Entry (EVar (mkIdentB i)) k
    qkinds = [
       -- The kinds are wired in (for now)
       (mkIdentB nameType,       [entry nameType sKind]),
       (mkIdentB nameConstraint, [entry nameConstraint sKind]),
       (mkIdentB nameSymbol,     [entry nameSymbol sKind]),
       (mkIdentB nameNat,        [entry nameNat sKind]),
       (identArrow,              [entry nameArrow sKindKindKind])
       ]
  in stFromList (map (first unQualIdent) qkinds) qkinds

-- E.g.
--  Bool  :: Type
--  Int   :: Type
--  (->)  :: Type -> Type -> Type
--  (=>)  :: forall k . Constraint -> k -> k
--  Maybe :: Type -> Type
primTypes :: [(Ident, [Entry])]
primTypes =
  let
    entry i = Entry (EVar i)
    k = mkIdent "k"
    kv = EVar k
    kk = IdKind k sKind
    -- Tuples are polykinded since they need to handle both Type and Constraint
    -- (,) :: forall k . k -> k -> k
    -- etc.
    tuple n =
      let
        i = tupleConstr builtinLoc n
      in  (i, [entry i $ EForall True [kk] $ foldr kArrow kv (replicate n kv)])
    -- (=>) :: forall k . Constraint -> k -> k
    kImplies = EForall True [kk] $ kConstraint `kArrow` (kv `kArrow` kv)
    -- (~) :: forall k . k -> k -> Constraint
    kTypeEqual = EForall True [kk] $ kv `kArrow` (kv `kArrow` kConstraint)
  in
      [
       -- The function arrow et al are bothersome to define in Primitives, so keep them here.
       -- But the fixity is defined in Primitives.
       (mkIdentB "->",           [entry identArrow    kTypeTypeTypeS]),
       (mkIdentB "=>",           [entry identImplies  kImplies]),
       (mkIdentB "~",            [entry identTypeEq   kTypeEqual]),
       -- Primitives.hs uses the type [], and it's annoying to fix that.
       -- XXX should not be needed
       (identList,               [entry identList     kTypeTypeS]),
       (mkIdentB "\x2192",       [entry identArrow    kTypeTypeTypeS]),  -- ->
       (mkIdentB "\x21d2",       [entry identImplies  kImplies])         -- =>
      ] ++
      map tuple (0 : enumFromTo 2 10)

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
      in  (c, [Entry (ECon $ ConData [(c, n)] c []) $ EForall True vks $ EForall True [] $ foldr tArrow r ts ])
  in  map tuple (0 : enumFromTo 2 10)

kArrow :: EKind -> EKind -> EKind
kArrow = tArrow

sArrow :: ESort -> ESort -> ESort
sArrow = tArrow

setUVar :: TRef -> EType -> T ()
setUVar i t = modify $ \ ts -> ts{ uvarSubst = IM.insert i t (uvarSubst ts) }

getUVar :: Int -> T (Maybe EType)
getUVar i = gets (IM.lookup i . uvarSubst)

munify :: HasCallStack =>
          SLoc -> Expected -> EType -> T ()
munify loc (Infer r) b = tSetRefType loc r b
munify loc (Check a) b = unify loc a b

expandSyn :: HasCallStack =>
             EType -> T EType
expandSyn at = do
  let
    syn ts t =
      case t of
        EApp f a -> do
          aa <- expandSyn a
          syn (aa:ts) f
        EVar i -> do
          syns <- gets synTable
          case M.lookup i syns of
            Nothing -> return $ eApps t ts
            Just (EForall _ vks tt) ->
              let s = zip (map idKindIdent vks) ts
                  lvks = length vks
                  lts = length ts
              in  case compare lvks lts of
                    LT -> expandSyn $ eApps (subst s tt) (drop lvks ts)
                    EQ -> expandSyn $ subst s tt
                    GT -> tcError (getSLoc i) $ "bad synonym use"
            Just _ -> impossible
        EUVar _ -> return $ eApps t ts
        ESign a _ -> expandSyn a   -- Throw away signatures, they don't affect unification
        EForall b iks tt | null ts -> EForall b iks <$> expandSyn tt
        ELit _ (LStr _) -> return t
        ELit _ (LInteger _) -> return t
        _ -> impossible
  syn [] at

mapEType :: (EType -> EType) -> EType -> EType
mapEType fn = rec
  where
    rec (EApp f a) = EApp (rec f) (rec a)
    rec (ESign t k) = ESign (rec t) k
    rec (EForall b iks t) = EForall b iks (rec t)
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
    EForall b iks t -> EForall b iks <$> derefUVar t
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

unifyR :: HasCallStack =>
          SLoc -> EType -> EType -> T ()
unifyR _   (EVar x1)    (EVar x2)      | x1 == x2  = return ()
unifyR loc (EApp f1 a1) (EApp f2 a2)               = do { unifyR loc f1 f2; unifyR loc a1 a2 }
unifyR loc t1@(EUVar r1) t2@(EUVar r2) | r1 < r2   = unifyVar loc r2 t1   -- always make higher
                                       | r1 > r2   = unifyVar loc r1 t2   --   TRefs point to lower
                                       | otherwise = return ()
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
--  tcTrace $ "unifyVar: " ++ show (r,t)
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
  if elem r1 vs then
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
      tcTrace ("TCMode=" ++ show tcm)
      tcTrace ("Value table:\n" ++ show env)
      tenv <- gets typeTable
      tcTrace ("Type table:\n" ++ show tenv)
-}
      tcError (getSLoc i) e

tLookupV :: HasCallStack =>
           Ident -> T (Expr, EType)
tLookupV i = do
  tcm <- gets tcMode
  tLookup (msgTCMode tcm) i

tInst :: HasCallStack => Expr -> EType -> T (Expr, EType)
tInst ae (EForall _ vks t) = do
  t' <- tInstForall vks t
  tInst ae t'
tInst ae at | Just (ctx, t) <- getImplies at = do
  --tcTrace $ "tInst: addConstraint: " ++ show ae ++ ", " ++ show d ++ " :: " ++ show ctx
  if eqExpr ae eCannotHappen then
    -- XXX Gruesome hack.  This avoid adding constraints in cases like
    --  (C a => a) -> T `subsCheck` b
    tInst ae t
   else do
    d <- newDictIdent (getSLoc ae)
    addConstraint d ctx
    tInst (EApp ae (EVar d)) t
tInst ae at = return (ae, at)

tInstForall :: [IdKind] -> EType -> T EType
tInstForall vks t =
  if null vks then
    return t
  else do
    let vs = map idKindIdent vks
    us <- mapM (const newUVar) vks
    return (subst (zip vs us) t)

tInst' :: EType -> T EType
tInst' (EForall _ vks t) = tInstForall vks t
tInst' t = return t

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
      venv'  = stInsertGlbQ qi [Entry e t] venv
      venv'' = stInsertGlbU  i [Entry e t] venv'
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

extData :: Ident -> EDef -> T ()
extData i d = do
  denv <- gets dataTable
  putDataTable (M.insert i d denv)

extFix :: Ident -> Fixity -> T ()
extFix i fx = modify $ \ ts -> ts{ fixTable = M.insert i fx (fixTable ts) }

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

tcDefs :: ImpType -> [EDef] -> T [EDef]
tcDefs impt ds = do
--  tcTrace ("tcDefs 1:\n" ++ showEDefs ds)
  mapM_ tcAddInfix ds
  dst <- tcDefsType ds
--  tcTrace ("tcDefs 2:\n" ++ showEDefs dst)
  mapM_ addTypeAndData dst
  dste <- tcExpandClassInst impt dst
--  tcTrace ("tcDefs 3:\n" ++ showEDefs dste)
  case impt of
    ImpNormal -> do
      setDefault dste
      dste' <- tcDefsValue dste
      mapM_ addAssocs dste'
      return dste'
    ImpBoot ->
      return dste

setDefault :: [EDef] -> T ()
setDefault defs = do
  tys <- gets typeTable
  ds <- sequence [ do { ts' <- mapM expandSyn ts; return (tyQIdent $ expLookup c tys, ts') }
                 | Default (Just c) ts <- defs ]
  dflts <- gets defaults
  let dflts' = foldr (uncurry M.insert) dflts ds
--  traceM $ "Active defaults " ++ show (M.toList dflts')
  putDefaults dflts'

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
--  tcTrace $ show vars
  vt <- gets valueTable
  let ent (Entry i t) = Entry i . mapEType def <$> derefUVar t
      def (EUVar _) = kType    -- default kind variables to Type
      def t = t
  vt' <- mapMSymTab ent vt
  putValueTable vt'
--  tcTrace $ "tcDefType value table:\n" ++ show vt'
  return dst

-- Get all kind signatures, and do sort checking of them.
getKindSigns :: HasCallStack => [EDef] -> T (M.Map EKind)
getKindSigns ds = do
  let iks = [ (i, k) | KindSign i k <- ds ]
      kind (i, k) = (,) i <$> tcKind (Check sKind) k
  multCheck (map fst iks)
  iks' <- mapM kind iks
  return $ M.fromList iks'

-- Expand class and instance definitions (must be done after type synonym processing)
tcExpandClassInst :: ImpType -> [EDef] -> T [EDef]
tcExpandClassInst impt dst = withTypeTable $ do
  dsc <- concat <$> mapM (expandClass impt) dst       -- Expand all class definitions
  dsf <- concat <$> mapM expandField dsc              -- Add HasField instances
--  tcTrace $ showEDefs dsf
  dsd <- concat <$> mapM doDeriving  dsf              -- Add derived instances
--  tcTrace $ showEDefs dsd
  dsi <- concat <$> mapM expandInst  dsd              -- Expand all instance definitions
  return dsi

-- Check&rename the given kinds, also insert the type variables in the symbol table.
withVks :: forall a . HasCallStack => [IdKind] -> ([IdKind] -> T a) -> T a
withVks vks fun = assertTCMode (>=TCType) $ do
  tcm <- gets tcMode
  let
    expect = case tcm of { TCType -> sKind; TCKind -> rSort; _ -> undefined }
    loop r [] = fun (reverse r)
    loop r (IdKind i mk : iks) = do
      -- When we have 'forall k (a :: k) . t' the k is a kind.
      -- Instead we have have to write 'forall (k :: Kind) (a :: k) . t' and then
      -- we have to guess that k needs sort checking.
      -- This doesn't work if there is not exactly 'Kind' (e.g., 'Primitives.Kind')
      -- This sucks.
      if tcm == TCType && guessIsKind mk then do
        s <- withTypeTable $ withTypeTable $ tcExpr (Check rSort) mk
        let ik = IdKind i s
        withExtTyps [ik] $ loop (ik : r) iks
      else do
        k' <- case mk of
                EVar d | d == dummyIdent -> newUVar
                _                        -> withTypeTable $ tcExpr (Check expect) mk   -- bump to next level
        withExtVal i k' $ loop (IdKind i k' : r) iks
  loop [] vks

guessIsKind :: EType -> Bool
guessIsKind (EVar i)                      = i == mkIdent "Kind"
guessIsKind t | Just (f, a) <- getArrow t = guessIsKind f || guessIsKind a
guessIsKind _                             = False

-- Add symbol a table entry (with kind) for each top level typeish definition.
-- If there is a kind signature, use it.  If not, use a kind variable.
addTypeKind :: M.Map EKind -> EDef -> T ()
addTypeKind kdefs adef = do
  let
    addDef (i, _) = do
      k <-
        case M.lookup i kdefs of
           Nothing -> newUVar
           Just k' -> return k'
      extValQTop i k
      
  case adef of
    Data    lhs _ _ -> addDef lhs
    Newtype lhs _ _ -> addDef lhs
    Type    lhs _   -> addDef lhs
    Class _ lhs _ _ -> addDef lhs
    _               -> return ()

-- Add symbols associated with a type.
addAssocs :: EDef -> T ()
addAssocs adef = do
  mn <- gets moduleName
  let
    addAssoc ti is = do
      vt <- gets valueTable
      let val i =
            case stLookup "" i vt of
              Right e -> ValueExport i e
              _       -> impossibleShow i
      addAssocTable (qualIdent mn ti) (map val is)

    assocData (Constr _ _ c (Left _)) = [c]
    assocData (Constr _ _ c (Right its)) = c : map fst its

  case adef of
    Data    (i, _) cs _ | not (isMatchDataTypeName i)
                        -> addAssoc i (nub $ concatMap assocData cs)
    Newtype (i, _) c  _ -> addAssoc i (assocData c)
    Class _ (i, _) _ ms -> addAssoc i [ x | Sign ns _ <- ms, m <- ns, x <- [m, mkDefaultMethodId m] ]
    _                   -> return ()

-- Add type synonyms to the synonym table, and data/newtype to the data table
addTypeAndData :: EDef -> T ()
addTypeAndData adef = do
  mn <- gets moduleName
  case adef of
    Type    (i, vs) t  -> extSyn  (qualIdent mn i) (EForall True vs t)
    Data    (i, _) _ _ -> extData (qualIdent mn i) adef
    Newtype (i, _) _ _ -> extData (qualIdent mn i) adef
    _                  -> return ()

-- Do kind checking of all typeish definitions.
tcDefType :: HasCallStack => EDef -> T EDef
tcDefType def = do
  --tcReset
  case def of
    Data    lhs cs ds      -> withLHS lhs $ \ lhs' -> flip (,) kType       <$> (Data    lhs'  <$> mapM tcConstr cs <*> mapM tcDerive ds)
    Newtype lhs c  ds      -> withLHS lhs $ \ lhs' -> flip (,) kType       <$> (Newtype lhs'  <$> tcConstr c       <*> mapM tcDerive ds)
    Type    lhs t          -> withLHS lhs $ \ lhs' -> first                    (Type    lhs') <$> tInferTypeT t
    Class   ctx lhs fds ms -> withLHS lhs $ \ lhs' -> flip (,) kConstraint <$> (Class         <$> tcCtx ctx <*> return lhs' <*> mapM tcFD fds <*> mapM tcMethod ms)
    Sign      is t         ->                                                  Sign      is   <$> tCheckTypeTImpl kType t
    ForImp ie i t          ->                                                  ForImp ie i    <$> tCheckTypeTImpl kType t
    Instance ct m          ->                                                  Instance       <$> tCheckTypeTImpl kConstraint ct <*> return m
    Default mc ts          ->                                                  Default (Just c) <$> mapM (tcDefault c) ts
                                                                                 where c = fromMaybe num mc
    Deriving ct            ->                                                  Deriving       <$> tCheckTypeTImpl kConstraint ct
    _                      -> return def
 where
   tcMethod (Sign is t) = Sign is <$> tCheckTypeTImpl kType t
   tcMethod (DfltSign i t) = DfltSign i <$> tCheckTypeTImpl kType t
   tcMethod m = return m
   tcFD (is, os) = (,) <$> mapM tcV is <*> mapM tcV os
     where tcV i = do { _ <- tLookup "fundep" i; return i }
   tcDerive = tCheckTypeT (kType `kArrow` kConstraint)
   num = mkBuiltin noSLoc "Num"
   tcDefault c t = do
     EApp _ t' <- tCheckTypeT kConstraint (EApp (EVar c) t)
     return t'

withLHS :: forall a . HasCallStack => LHS -> (LHS -> T (a, EKind)) -> T a
withLHS (i, vks) ta = do
  (_, ki) <- tLookupV i
  withVks vks $ \ vks' -> do
    (a, kr) <- ta (i, vks')
    let kapp = foldr kArrow kr (map (\ (IdKind _ k) -> k) vks')
    --unify (getSLoc i) ki kapp
    _ <- subsCheckRho (getSLoc i) eCannotHappen ki kapp
    return a

tcCtx :: HasCallStack => [EConstraint] -> T [EConstraint]
tcCtx = mapM (tCheckTypeT kConstraint)

tcConstr :: HasCallStack => Constr -> T Constr
tcConstr (Constr iks ct c ets) =
  assertTCMode (==TCType) $
  withVks iks $ \ iks' ->
    Constr iks' <$> tcCtx ct <*> pure c <*>
      either (\ x -> Left  <$> mapM (\ (s,t)     ->         (,)s  <$> tcTypeT (Check kType) t) x)
             (\ x -> Right <$> mapM (\ (i,(s,t)) -> ((,)i . (,)s) <$> tcTypeT (Check kType) t) x) ets

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
expandClass :: ImpType -> EDef -> T [EDef]
expandClass impt dcls@(Class ctx (iCls, vks) fds ms) = do
  mn <- gets moduleName
  let
      meths = [ b | b@(Sign _ _) <- ms ]
      methIds = concatMap (\ (Sign is _) -> is) meths
      mdflts = [ (i, eqns) | Fcn i eqns <- ms ]
      dflttys = [ (i, t) | DfltSign i t <- ms ]
      tCtx = tApps (qualIdent mn iCls) (map (EVar . idKindIdent) vks)
      mkDflt (Sign is t) = concatMap method is
        where method methId = [ Sign [iDflt] $ EForall True vks $ tCtx `tImplies` ty, def $ lookup methId mdflts ]
                where ty = fromMaybe t $ lookup methId dflttys
                      def Nothing = Fcn iDflt $ simpleEqn noDflt
                      def (Just eqns) = Fcn iDflt eqns
                      iDflt = mkDefaultMethodId methId
                      noDflt = mkExn (getSLoc methId) (unIdent methId) "noMethodError"
              
      mkDflt _ = impossible
      dDflts = case impt of
                 ImpNormal -> concatMap mkDflt meths
                 ImpBoot   -> []
  -- Add to the class table.  XXX also in addValueClass???
  addClassTable (qualIdent mn iCls)
                (ClassInfo vks ctx (EUVar 0) methIds (mkIFunDeps (map idKindIdent vks) fds))   -- Initial entry, no type needed.
  return $ dcls : dDflts
expandClass _ d = return [d]

simpleEqn :: Expr -> [Eqn]
simpleEqn e = [Eqn [] $ simpleAlts e]

simpleAlts :: Expr -> EAlts
simpleAlts e = EAlts [([], e)] []

-- Keep the list empty if there are no fundeps
mkIFunDeps :: [Ident] -> [FunDep] -> [IFunDep]
--mkIFunDeps vs [] = [(map (const True) vs, map (const False) vs)]
mkIFunDeps vs fds = map (\ (is, os) -> (map (`elem` is) vs, map (`elem` os) vs)) fds

-- Turn (unqualified) class and method names into a default method name
mkDefaultMethodId :: Ident -> Ident
mkDefaultMethodId meth = addIdentSuffix meth defaultSuffix

isDefaultMethodId :: Ident -> Bool
isDefaultMethodId i = defaultSuffix `isSuffixOf` unIdent i

defaultSuffix :: String
defaultSuffix = uniqIdentSep ++ "dflt"

splitInst :: EConstraint -> ([IdKind], [EConstraint], EConstraint)
splitInst (EForall _ iks t) =
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
  let iInst = mkInstId loc cc
      sign = Sign [iInst] act
--  tcTrace ("expandInst " ++ show iInst)
--  (e, _) <- tLookupV iCls
  ct <- gets classTable
--  let qiCls = getAppCon e
  (ClassInfo _ supers _ mis fds) <-
    case M.lookup qiCls ct of
      Nothing -> tcError loc $ "not a class " ++ showIdent qiCls
      Just x -> return x
  -- XXX this ignores type signatures and other bindings
  -- XXX should tack on signatures with ESign
  let clsMdl = qualOf qiCls                   -- get class's module name
      ies = [(i, ELam noSLoc qs) | Fcn i qs <- bs]
      meth i = fromMaybe (ELam noSLoc $ simpleEqn $ EVar $ setSLocIdent loc $ mkDefaultMethodId $ qualIdent clsMdl i) $ lookup i ies
      meths = map meth mis
      sups = map (const (EVar $ mkIdentSLoc loc dictPrefixDollar)) supers
      args = sups ++ meths
      instBind (Fcn i _) = i `elem` mis
      instBind _ = False
  case filter (not . instBind) bs of
    [] -> return ()
    b:_ -> tcError (getSLoc b) $ "superflous instance binding"
  let bind = Fcn iInst $ eEqns [] $ eApps (EVar $ mkClassConstructor qiCls) args
  mn <- gets moduleName
  addInstTable [(EVar $ qualIdent mn iInst, vks, ctx, cc, fds)]
  return [dinst, sign, bind]
expandInst d = return [d]

---------------------

tcDefsValue :: [EDef] -> T [EDef]
tcDefsValue defs = do
--  tcTrace $ "tcDefsValue: ------------ start"
  -- Gather up all type signatures, and put them in the environment.
  mapM_ addValueType defs
  let smap = M.fromList $ [ (i, ()) | Sign is _ <- defs, i <- is ]
      -- Split Fcn into those without and with type signatures
      unsigned = filter noSign defs
        where noSign (Fcn i _) = hasNoSign i
              noSign (Pattern (i, _) _ _) = hasNoSign i
              noSign (PatBind p _) = any hasNoSign (patVars p)
              noSign _ = False
              hasNoSign i = isNothing $ M.lookup i smap
      -- split the unsigned defs into strongly connected components
      sccs = stronglyConnComp $ map node unsigned
        where node d@(Fcn i e)             = (d, i,                tr $ allVarsEqns e)
              node d@(Pattern (i, _) p me) = (d, i,                tr $ allVarsPat p $ maybe [] allVarsEqns me)
              node d@(PatBind p e)         = (d, head $ patVars p, tr $ allVarsExpr e)  -- use the first bound var as the key
              node _ = undefined
              tr x | null sub = x  -- do nothing when there are no PatBinds
                   | otherwise = map (\ i -> fromMaybe i $ lookup i sub) x
              -- Map all (bound) identifiers in a PatBind into the first (bound) identifier
              sub = [ (d, i) | PatBind p _ <- unsigned, i:ds <- [patVars p], d <- ds ]
      tcSCC (AcyclicSCC d@(Pattern _ _ _)) = tcPatSyn d
      tcSCC (AcyclicSCC d) = tInferDefs smap [d]
      tcSCC (CyclicSCC ds) = tInferDefs smap ds
  --traceM $ "tcDefsValue: unsigned=" ++ show unsigned
  -- type infer and enter each SCC in the symbol table
  -- return inferred Sign
  signDefs <- mapM tcSCC sccs
  defs' <- concat <$> mapM expandPatSyn defs
--  traceM $ "tcDefsValue: ------------ expandPatSyn"
--  traceM $ showEDefs defs'
--  tcTrace $ "tcDefsValue: ------------ check"
  --  type check all definitions (the inferred ones will be rechecked)
  defs'' <- mapM (\ d -> do { tcReset; tcDefValue d}) defs'
  let defs''' = concat signDefs ++ defs''
--  traceM $ "tcDefsValue: ------------ done"
--  traceM $ showEDefs defs'''
  pure defs'''

-- Infer a type for a definition
tInferDefs :: M.Map () -> [EDef] -> T [EDef]
tInferDefs smap fcns = do
--  traceM "tInferDefs"
  tcReset
  -- Invent type variables for the definitions
  xts <- 
           let f (Fcn i _)            = do t <- newUVar; pure [(i, t)]
               f (Pattern (i, _) _ _) = do t <- newUVar; pure [(i, t)]
               f (PatBind p _)        = concat <$> mapM g (patVars p)
               f _                    = impossible
               -- Only add type variables for those variables that don't have a signature
               g i = case M.lookup i smap of
                       Nothing -> do t <- newUVar; pure [(i, t)]
                       _       -> pure []
           in  concat <$> mapM f fcns
  --tcTrace $ "tInferDefs: " ++ show (map fst xts)
  -- Temporarily extend the local environment with the type variables
  withExtVals xts $ do
    -- Infer types for all the Fcns, ignore the new bodies.
    -- The bodies will be re-typecked in tcDefsValues.
    let tc (Fcn _ eqns) (_, t)   = do tcEqns False t eqns; return ()
        tc (Pattern (i,_) _ _) _ = tcError (getSLoc i) "Cannot infer recursive pattern synonym types"
        tc (PatBind p e)       _ = do tcPatBind PatBind p e; return ()
        tc _ _ = impossible
    zipWithM_ tc fcns xts
  -- Get the unsolved constraints
  ctx <- getUnsolved
  -- For each definition, quantify over the free meta variables, and include
  -- context mentioning them.
  let genTop :: (Ident, EType) -> T EDef
      genTop (i, t) = do
        t' <- derefUVar t
        let vs = metaTvs [t']
            ctx' = filter (\ c -> not (null (intersect vs (metaTvs [c])))) ctx
            t'' = addConstraints ctx' t'
            vs' = metaTvs [t'']
        t''' <- quantify vs' t''
        --tcTrace $ "tInferDefs: " ++ showIdent i ++ " :: " ++ showEType t'''
        extValQTop i t'''
        return $ Sign [i] t'''
  mapM genTop xts

getUnsolved :: T [EConstraint]
getUnsolved = do
  _ <- solveConstraints
  ctx <- gets (map snd . constraints)
  ctx' <- mapM derefUVar ctx
  putConstraints []
  return $ nubBy eqEType ctx'

addValueType :: EDef -> T ()
addValueType adef = do
  mn <- gets moduleName
  -- tcTrace ("addValueType: " ++ showEDefs [adef])
  let addConFields _     (Constr _ _ _ (Left _)) = return ()
      addConFields tycon (Constr _ _ _ (Right fs)) = mapM_ addField fs
        where addField (fld, _) = do
                (fe, fty) <- tLookup "???" $ mkGetName tycon fld
                extValETop fld fty fe
  case adef of
    Sign is@(i:_) t | isConIdent i -> do
      -- pattern synonym
      t' <- canonPatSynType t
      mapM_ (addPatSyn t') is
    Sign is t ->
      -- regular synonym
      mapM_ (\ i -> extValQTop i t) is
    Data (tycon, vks) cs _ -> do
      let
        cti = [ (qualIdent mn c, either length length ets + if null ctx then 0 else 1) | Constr _ ctx c ets <- cs ]
        tret = tApps (qualIdent mn tycon) (map tVarK vks)
        addCon (Constr evks ectx c ets) = do
          let ts = either id (map snd) ets
              cty = EForall True vks $ EForall True evks $ addConstraints ectx $ foldr (tArrow . snd) tret ts
              fs = either (const []) (map fst) ets
          extValETop c cty (ECon $ ConData cti (qualIdent mn c) fs)
      mapM_ addCon cs
      mapM_ (addConFields tycon) cs
    Newtype (tycon, vks) con@(Constr _ _ c ets) _ -> do
      let
        t = snd $ head $ either id (map snd) ets
        tret = tApps (qualIdent mn tycon) (map tVarK vks)
        fs = either (const []) (map fst) ets
      extValETop c (EForall True vks $ EForall True [] $ tArrow t tret) (ECon $ ConNew (qualIdent mn c) fs)
      addConFields tycon con
    ForImp _ i t -> extValQTop i t
    Class ctx (i, vks) fds ms -> addValueClass ctx i vks fds ms
    _ -> return ()

-- Add a pattern synonym to the symbol table.
addPatSyn :: EType -> Ident -> T ()
addPatSyn at i = do
  mn <- gets moduleName
  let (_, _, _, _, t) = splitPatSynType at
      n = length $ fst $ getArrows t
      qi = qualIdent mn i
      qip = mkPatSynMatch qi
      mtch = (EVar qip, mkPatSynMatchType qip at)
  extValETop i at $ ECon $ ConSyn qi n mtch

-- XXX FunDep
addValueClass :: [EConstraint] -> Ident -> [IdKind] -> [FunDep] -> [EBind] -> T ()
addValueClass ctx iCls vks fds ms = do
  mn <- gets moduleName
  let
      meths = [ b | b@(Sign _ _) <- ms ]
      methTys = map (\ (Sign _ t) -> t) meths
      methIds = concatMap (\ (Sign is _) -> is) meths
      supTys = ctx  -- XXX should do some checking
      targs = supTys ++ methTys
      qiCls = qualIdent mn iCls
      tret = tApps qiCls (map tVarK vks)
      cti = [ (qualIdent mn iCon, length targs) ]
      iCon = mkClassConstructor iCls
      iConTy = EForall True vks $ foldr tArrow tret targs
  extValETop iCon iConTy (ECon $ ConData cti (qualIdent mn iCon) [])
  let addMethod (Sign is t) = mapM_ method is
        where method i = extValETop i (EForall True vks $ tApps qiCls (map (EVar . idKindIdent) vks) `tImplies` t) (EVar $ qualIdent mn i)
      addMethod _ = impossible
--  tcTrace ("addValueClass " ++ showEType (ETuple ctx))
  mapM_ addMethod meths
  -- Update class table, now with actual constructor type.
  addClassTable qiCls (ClassInfo vks ctx iConTy methIds (mkIFunDeps (map idKindIdent vks) fds))

mkClassConstructor :: Ident -> Ident
mkClassConstructor i = addIdentSuffix i "$C"

tcDefValue :: HasCallStack =>
              EDef -> T EDef
tcDefValue adef =
  assertTCMode (==TCExpr) $
  case adef of
    Fcn i eqns -> do
      (_, t) <- tLookup "type signature" i
      t' <- expandSyn t
--      when (isConIdent i) $ do
--        tcTrace $ "tcDefValue: patsyn\n" ++ show i ++ " :: " ++ show t'
--        tcTrace $ "tcDefValue:\n" ++ showEDefs [adef]
--      tcTrace $ "tcDefValue: ------- start " ++ showIdent i
--      tcTrace $ "tcDefValue: " ++ showIdent i ++ " :: " ++ showExpr t'
--      tcTrace $ "tcDefValue: " ++ showEDefs [adef]
      teqns <- tcEqns True t' eqns
--      tcTrace ("tcDefValue: after\n" ++ showEDefs [adef, Fcn i teqns])
--      cs <- gets constraints
--      tcTrace $ "tcDefValue: constraints: " ++ show cs
      checkConstraints
      mn <- gets moduleName
--      tcTrace $ "tcDefValue: " ++ showIdent i ++ " done"
      return $ Fcn (qualIdent mn i) teqns
    PatBind p e -> tcPatBind PatBind p e
    ForImp ie i t -> do
      mn <- gets moduleName
      t' <- expandSyn t
      return (ForImp ie (qualIdent mn i) t')
    Pattern _ _ _ -> impossible
    _ -> return adef

-- This is only used during inference.
-- When doing type checking the actual Pattern definition will have been
-- removed by expandPatSyn.
-- The important thing here is the call to addPatSyn
tcPatSyn :: EDef -> T [EDef]
tcPatSyn (Pattern (ip, vks) p me) = do
--  traceM $ "tcPatSyn: enter " ++ show (ip, vks, p, me)
  let step [] t = tcPat (Check t) p
      step (ik:iks) t = do
        (ti, tr) <- unArrow (getSLoc ik) t
        withExtVal (idKindIdent ik) ti $ step iks tr
  pty <- newUVar   -- invent a type
  (sks, dicts, _p) <- step vks pty
  let ctx2 = map snd dicts
--  traceM $ "tcPatSyn: pat " ++ show (sks, ctx2)
  case me of Nothing -> pure (); Just e -> void $ tcEqns False pty e
  ctx1 <- getUnsolved
--  traceM $ "tcPatSyn: ctx " ++ show ctx1
  ty0 <- addConstraints ctx2 <$> derefUVar pty
  let ctx1' = deleteFirstsBy eqEType ctx1 ctx2   -- remove provided from required
      (sks', sub) = tyVarSubst sks ty0           -- turn skolems
      ty1 = subst sub ty0                        --    into rigid tyvars
  ty2 <- quantify (metaTvs [ty1]) (addConstraints ctx1' ty1)
  let (vs, ty3) = unForall ty2
      ty4 = eForall' False (sks' ++ vs) ty3      -- add the skolems tyvars
  ty5 <- canonPatSynType ty4
--  traceM $ "tcPatSyn: tys " ++ show (ty0, ty1, ty2, ty3, ty4, ty5)
  addPatSyn ty5 ip
--  traceM ("tcPatSyn: after " ++ show (ip, ty5))
  return [ Sign [ip] ty3 ]
tcPatSyn _ = impossible

-- Add implicit forall and type check.
tCheckTypeTImpl :: HasCallStack => EType -> EType -> T EType
tCheckTypeTImpl tchk t@(EForall _ _ _) = tCheckTypeT tchk t
tCheckTypeTImpl tchk t = do
  bvs <- stKeysLcl <$> gets valueTable         -- bound outside
  let fvs = freeTyVars [t]                     -- free variables in t
      -- these are free, and need quantification.  eDummy indicates missing kind
      iks = map (\ i -> IdKind i eDummy) (fvs \\ bvs)
  --when (not (null iks)) $ tcTrace ("tCheckTypeTImpl: " ++ show (t, eForall iks t))
  tCheckTypeT tchk (eForall' False iks t)

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
-- When checking, the expected type is simply given.
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
--  tcTrace $ "tCheckExpr: " ++ show (e, ctx, t')
  d <- newADictIdent (getSLoc e)
  e' <- withDict d ctx $ tCheckExprAndSolve t' e
  return $ eLam [EVar d] e'

tCheckExpr t e = tCheck tcExpr t e

tGetRefType :: HasCallStack =>
               TRef -> T EType
tGetRefType ref = do
  m <- gets uvarSubst
  case IM.lookup ref m of
    Nothing -> return (EUVar ref)
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

tcExpr :: HasCallStack =>
          Expected -> Expr -> T Expr
tcExpr mt ae = do
--  tcTrace ("tcExpr enter: mt=" ++ show mt ++ " ae=" ++ showExpr ae)
  r <- tcExprR mt ae
--  tcTrace ("tcExpr exit: " ++ showExpr r)
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

           | isDummyIdent i -> tcError loc "_ cannot be used as a variable"
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
--             tcTrace $ "EVar: " ++ showIdent i ++ " :: " ++ showExpr t ++ " = " ++ showExpr t' ++ " mt=" ++ show mt
             instSigma loc e t' mt
    EQVar e t ->  -- already resolved, just instantiate
             instSigma loc e t mt

    EApp f a -> do
--      tcTrace $ "txExpr(0) EApp: expr=" ++ show ae ++ ":: " ++ show mt
      (f', ft) <- tInferExpr f
      -- A hack to make $ work the same way with instantiation as application.
      -- This is ugly, but GHC does it, so people use it.
      -- So use the identity '($) a == a'
      case f' of
        EVar i | i == mkIdent "Data.Function.$" -> tcExpr mt a
        _ -> do
--          tcTrace $ "tcExpr(1) EApp: f=" ++ show f ++ "; f'=" ++ showExprRaw f' ++ " :: " ++ show ft
          (at, rt) <- unArrow loc ft
--          tcTrace $ "tcExpr(2) EApp: f=" ++ show f ++ " :: " ++ show ft ++ ", arg=" ++ show a ++ " :: " ++ show at ++ " retty=" ++ show rt
          -- We want to do the unification of rt ant mt before checking the argument to
          -- have more type information.  See tests/Eq1.hs.
          -- But instSigma may transform the input expression, so we have to be careful.
          let etmp = EUVar ugly
              ugly = -1::Int
          etmp' <- instSigma loc etmp rt mt
          a' <- checkSigma a at
--          tcTrace $ "tcExpr(3) EApp: f = " ++ show f ++ " :: " ++ show ft ++ ", arg=" ++ show a' ++ " :: " ++ show at ++ " retty=" ++ show rt ++ " mt = " ++ show mt
          let res = EApp f' a'
          case etmp' of
            EUVar _ -> return res   -- instSigma did nothing, this is the common case
            _ -> return $ substEUVar [(ugly, res)] etmp'

    EOper e ies -> tcOper e ies >>= tcExpr mt
    ELam _ qs -> tcExprLam mt loc qs
    ELit _ lit -> do
      tcm <- gets tcMode
      case tcm of
        TCType ->
          case lit of
            LStr _ -> instSigma loc (ELit loc lit) (tConI loc nameSymbol) mt
            LInteger _ -> instSigma loc (ELit loc lit) (tConI loc nameNat) mt
            _      -> impossible
        TCExpr -> do
          let getExpected (Infer _) = pure Nothing
              getExpected (Check t) = Just <$> (derefUVar t >>= expandSyn)
          case lit of
            LInteger i -> do
              mex <- getExpected mt
              case mex of
                -- Convert to Int in the compiler, that way (99::Int) will never involve fromInteger
                -- (which is not always in scope).
                Just (EVar v) | v == identInt     -> tcLit  mt loc (LInt (fromInteger i))
                              | v == identWord    -> tcLit' mt loc (LInt (fromInteger i)) (tConI loc nameWord)
                              | v == identFloatW  -> tcLit  mt loc (LDouble (fromInteger i))
                              | v == identInteger -> tcLit  mt loc lit
                _ -> do
                  (f, ft) <- tInferExpr (EVar (mkBuiltin loc "fromInteger"))
                  (_at, rt) <- unArrow loc ft
                  -- We don't need to check that _at is Integer, it's part of the fromInteger type.
                  instSigma loc (EApp f ae) rt mt
            LRat r -> do
              mex <- getExpected mt
              case mex of
                Just (EVar v) | v == mkIdent nameFloatW -> tcLit mt loc (LDouble (fromRational r))
                _ -> do
                  (f, ft) <- tInferExpr (EVar (mkBuiltin loc "fromRational"))
                  (_at, rt) <- unArrow loc ft
                  -- We don't need to check that _at is Rational, it's part of the fromRational type.
                  instSigma loc (EApp f ae) rt mt
            -- This implements OverloadedStrings.  It causes a small slowdown (2%)
            LStr _ -> do
              mex <- getExpected mt
              case mex of
                Just (EApp (EVar lst) (EVar c))
                 | lst == identList, c == identChar -> tcLit mt loc lit
                _ -> do
                  (f, ft) <- tInferExpr (EVar (mkBuiltin loc "fromString"))
                  (_at, rt) <- unArrow loc ft
                  -- We don't need to check that _at is String, it's part of the fromString type.
                  --tcTrace ("LStr " ++ show (loc, r))
                  instSigma loc (EApp f ae) rt mt
            -- Not LInteger, LRat, LStr
            _ -> tcLit mt loc lit
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
    EParen e -> tcExpr mt e
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
              nofail <- failureFree p
              let
                ibind = mkBuiltin loc ">>="
                sbind = maybe ibind (\ mn -> qualIdent mn ibind) mmn
                x = eVarI loc "$b"
                patAlt = [(p, simpleAlts $ EDo mmn ss)]
                failMsg s = EApp (EVar (mkBuiltin loc "fail")) (ELit loc (LStr s))
                failAlt =
                  if nofail then []
                  else [(eDummy, simpleAlts $ failMsg "bind")]
              tcExpr mt (EApp (EApp (EVar sbind) a)
                              (eLam [x] (ECase x (patAlt ++ failAlt))))
            SThen a -> do
              let
                ithen = mkBuiltin loc ">>"
                sthen = maybe ithen (\ mn -> qualIdent mn ithen) mmn
              tcExpr mt (EApp (EApp (EVar sthen) a) (EDo mmn ss))
                
            SLet bs ->
              tcExpr mt (ELet bs (EDo mmn ss))

    ESectL e i -> tcLSect e i >>= tcExpr mt
    ESectR i e -> tcRSect i e >>= tcExpr mt
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

    -- Translate (if | a1; | a2 ...) into
    --           (case [] of _ | a1; | a2 ...)
    EMultiIf a ->
      tcExpr mt $ ECase (EListish (LList [])) [(EVar (mkIdent "_"), a)]

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
    EForall b vks t ->
--      assertTCMode (==TCType) $
      withVks vks $ \ vks' -> do
        tt <- tcExpr mt t
        return (EForall b vks' tt)
    EUpdate e flds -> do
      ises <- concat <$> mapM (dsEField e) flds
      me <- dsUpdate unsetField e ises
      case me of
        Just e' -> tcExpr mt e'
        Nothing -> tcExpr mt $ foldr eSetFields e ises
    ESelect is -> do
        let x = eVarI loc "$x"
        tcExpr mt $ eLam [x] $ foldl (\ e i -> EApp (eGetField i) e) x is
    _ -> error $ "tcExpr: cannot handle: " ++ show (getSLoc ae) ++ " " ++ show ae
      -- impossible

-- Approximation of failure free.
-- XXX single constructor types should be transparent
failureFree :: EPat -> T Bool
failureFree p@(EVar _) = failureFreeAp [] p
failureFree p@(EApp _ _) = failureFreeAp [] p
failureFree (ETuple ps) = and <$> mapM failureFree ps
failureFree (ESign p _) = failureFree p
failureFree (EAt _ p) = failureFree p
failureFree (ELazy True _) = return True
failureFree (ELazy False p) = failureFree p
failureFree (EViewPat _ p) = failureFree p
failureFree (EParen p) = failureFree p
failureFree _ = return False

failureFreeAp :: [Bool] -> EPat -> T Bool
failureFreeAp bs (EApp f a) = do
  b <- failureFree a
  failureFreeAp (b:bs) f
failureFreeAp bs (EVar v) | not (isConIdent v) = return True
                          | otherwise = do
                              (con, _) <- tLookupV v
                              return $ case con of
                                ECon (ConNew _ _) -> and bs
                                ECon (ConData [_] _ _) -> and bs  -- single constructor
                                _ -> False
failureFreeAp bs (ESign p _) = failureFreeAp bs p
failureFreeAp _ _ = return False  -- bad pattern, just ignore
                           
eSetFields :: EField -> Expr -> Expr
eSetFields (EField is e) r =
  let loc = getSLoc is
      eCompose = EVar $ mkBuiltin loc "composeSet"
      has = map eHasField $ init is
      set1 = eSetField (last is)
      set = foldr (EApp . EApp eCompose) set1 has
  in  EApp (EApp set r) e
eSetFields _ _ = impossible

eHasField :: Ident -> Expr
eHasField i = EApp (EVar ihas) (eProxy i)
  where ihas = mkBuiltin (getSLoc i) "hasField"

eSetField :: Ident -> Expr
eSetField i = EApp (EVar iset) (eProxy i)
  where iset = mkBuiltin (getSLoc i) "setField"

eGetField :: Ident -> Expr
eGetField i = EApp (EVar iget) (eProxy i)
  where iget = mkBuiltin (getSLoc i) "getField"

eProxy :: Ident -> Expr
eProxy i = ESign proxy (EApp proxy (ELit loc (LStr (unIdent i))))
  where proxy = EVar $ mkBuiltin loc "Proxy"
        loc = getSLoc i

dsEField :: Expr -> EField -> T [EField]
dsEField _ e@(EField _ _) = return [e]
dsEField _ (EFieldPun is) = return [EField is $ EVar (last is)]
dsEField e EFieldWild = do
  (e', _) <- tInferExpr e
  case e' of
    ECon c -> return [ EField [f] (EVar f) | f <- conFields c ]
    _ -> tcError (getSLoc e) "record wildcard not allowed"

-- Patterns need to expand EFieldWild before type checking
dsEFields :: EPat -> T EPat
dsEFields apat =
  case apat of
    EVar _ -> return apat
    EApp p1 p2 -> EApp <$> dsEFields p1 <*> dsEFields p2
    EOper p1 ips -> EOper <$> dsEFields p1 <*> mapM (\ (i, p2) -> (,) i <$> dsEFields p2) ips
    ELit _ _ -> return apat
    ETuple ps -> ETuple <$> mapM dsEFields ps
    EListish (LList ps) -> EListish . LList <$> mapM dsEFields ps
    ESign p t -> ESign <$> dsEFields p <*> pure t
    EAt i p -> EAt i <$> dsEFields p
    EViewPat e p -> EViewPat e <$> dsEFields p
    ELazy z p -> ELazy z <$> dsEFields p
    ECon _ -> return apat
    EUpdate c fs -> EUpdate c . concat <$> mapM (dsEField c) fs
    EParen p -> dsEFields p
    ENegApp _ -> return apat
    EOr ps -> EOr <$> mapM dsEFields ps
    _ -> error $ "dsEFields " ++ show apat

unsetField :: Ident -> Expr
unsetField i = mkExn (getSLoc i) (unIdent i) "recConError"

dsUpdate :: (Ident -> Expr) -> Expr -> [EField] -> T (Maybe Expr)
dsUpdate unset e flds = do
  (e', _) <- tInferExpr e
  case e' of
    ECon c -> do
      let ises = map unEField flds
          fs = conFields c
          ies = map (first head) ises
          is = map fst ies
          as = map field fs
          field i = fromMaybe (unset i) $ lookup i ies
      case filter ((> 1) . length . fst) ises of
        (i:_, _):_ -> tcError (getSLoc i) "Nested fields not allowed"
        _ -> return ()
      case is \\ fs of
        vs@(v:_) -> tcError (getSLoc v) $ "extra field(s) " ++ unwords (map unIdent vs)
        _ -> return ()
      return $ Just $ eApps e as
    _ -> return Nothing

enum :: SLoc -> String -> [Expr] -> Expr
enum loc f = eApps (EVar (mkBuiltin loc ("enum" ++ f)))

tcLit :: HasCallStack => Expected -> SLoc -> Lit -> T Expr
tcLit mt loc l@(LPrim _) = newUVar >>= tcLit' mt loc l
tcLit mt loc l@(LExn  _) = newUVar >>= tcLit' mt loc l
tcLit mt loc l = do
  let t =
        case l of
          LInt _     -> tConI loc nameInt
          LInteger _ -> tConI loc nameInteger
          LDouble _  -> tConI loc nameFloatW
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
      let fx = getFixity fixs (getAppCon ei)
      return ((EVar i, fx), e)

  ites <- mapM opfix aies
  case resolveFixity ae ites of
    Left (loc, err) -> tcError loc err
    Right e -> return e

tcLSect :: Expr -> Ident -> T Expr
tcLSect (EOper e ies) op = do
  let x = eVarI loc "$x"
      loc = getSLoc op
  e' <- tcOper e (ies ++ [(op, x)])
  case e' of
    EApp f x' | x' `eqExpr` x -> return f
    _                   -> tcError loc "Bad section fixity"
tcLSect e op =
  return (EApp (EVar op) e)

tcRSect :: Ident -> Expr -> T Expr
tcRSect op (EOper e ies) = do
  let x = eVarI loc "$x"
      loc = getSLoc op
  e' <- tcOper x ((op, e):ies)
  case e' of
    EApp (EApp _ x') _ | x `eqExpr` x' -> return (eLam [x] e')
    _                            -> tcError loc "Bad section fixity"
tcRSect op e = do
  let x = eVarI (getSLoc op) "$x"
  return (eLam [x] (EApp (EApp (EVar op) x) e))

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

tcExprLam :: Expected -> SLoc -> [Eqn] -> T Expr
tcExprLam mt loc qs = do
  t <- tGetExpType mt
  ELam loc <$> tcEqns False t qs

tcEqns :: Bool -> EType -> [Eqn] -> T [Eqn]
--tcEqns _ t eqns | trace ("tcEqns: " ++ showEBind (Fcn dummyIdent eqns) ++ " :: " ++ show t) False = undefined
tcEqns top (EForall expl iks t) eqns | expl      = withExtTyps iks $ tcEqns top t eqns
                                     | otherwise =                   tcEqns top t eqns
tcEqns top t eqns | Just (ctx, t') <- getImplies t = do
  let loc = getSLoc eqns
  d <- newADictIdent loc
  f <- newIdent loc "fcnD"
  withDict d ctx $ do
    eqns' <- tcEqns top t' eqns
    let eqn =
          case eqns' of
            [Eqn [] alts] -> Eqn [EVar d] alts
            _             -> Eqn [EVar d] $ EAlts [([], EVar f)] [Fcn f eqns']
    return [eqn]
tcEqns top t eqns = do
  let loc = getSLoc eqns
  f <- newIdent loc "fcnS"
  (eqns', ds) <- solveAndDefault top $ mapM (tcEqn t) eqns
--  tcTrace $ "tcEqns done: " ++ showEBind (Fcn dummyIdent eqns')
  case ds of
    [] -> return eqns'
    _  -> do
      let
        bs = eBinds ds
        eqn = Eqn [] $ EAlts [([], EVar f)] (bs ++ [Fcn f eqns'])
      return [eqn]

tcEqn :: EType -> Eqn -> T Eqn
--tcEqn t eqn | trace ("tcEqn: " ++ show eqn ++ " :: " ++ show t) False = undefined
tcEqn t eqn =
  case eqn of
    Eqn ps alts -> tcPats t ps $ \ t' ps' -> do
--      tcTrace $ "tcEqn " ++ show ps ++ " ---> " ++ show ps'
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
--    tcTrace ("tcAlts: bs out " ++ showEBinds bbs)
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
eBinds ds = [Fcn i $ simpleEqn e | (i, e) <- ds]

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
--  tcTrace $ "tCheckPatC: " ++ show app ++ " :: " ++ show t
  app' <- dsEFields app
  let vs = patVars app'
  multCheck vs
  env <- mapM (\ v -> (,) v <$> newUVar) vs
  withExtVals env $ do
    (_sks, ds, pp) <- tCheckPat t app'
--    tcTrace ("tCheckPatC: " ++ show pp)
--    xt <- derefUVar t
--    tcTrace ("tCheckPatC ds=" ++ show ds ++ "t=" ++ show xt)
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
  let loc = getSLoc ae
      lit = tcPat mt (EViewPat (EApp (EVar (mkBuiltin loc "==")) ae) (EVar (mkBuiltin loc "True")))
      isNeg (EVar i) = i == mkIdent "negate"
      isNeg _ = False
  in
  case ae of
    EVar i | isDummyIdent i -> do
               -- _ can be anything, so just ignore it
               _ <- tGetExpType mt
               return ([], [], ae)
           | not (isConIdent i) -> do
               -- All pattern variables are in the environment as
               -- type references.  Assign the reference the given type.
               ext <- tGetExpType mt
               (p, t) <- tLookupV i
               unify loc t ext
               return ([], [], p)
           | otherwise -> tcPatAp mt [] ae
    EQVar _ _ -> tcPatAp mt [] ae
    EApp f _
           | isNeg f   -> lit            -- if it's (negate e) it must have been a negative literal
           | otherwise -> tcPatAp mt [] ae

    EOper e ies -> do e' <- tcOper e ies; tcPat mt e'

    ETuple es -> do
      let
        n = length es
      (xs, tes) <- fmap unzip (mapM tInferPat es)
      let
        (sks, ds, ees) = unzip3 xs
        ttup = tApps (tupleConstr loc n) tes
      munify loc mt ttup
      return (concat sks, concat ds, ETuple ees)

    EParen e -> tcPat mt e

    EListish (LList es) -> do
      te <- newUVar
      munify loc mt (tApp (tList loc) te)
      xs <- mapM (tCheckPat te) es
      let !(sks, ds, es') = unzip3 xs
      return (concat sks, concat ds, EListish (LList es'))

    ELit _ _ -> lit

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

    ELazy z p -> do
      (sk, d, p') <- tcPat mt p
      return (sk, d, ELazy z p')

    -- Allow C{} syntax even for non-records
    EUpdate p [] -> do
      (p', _) <- tInferExpr p
      case p' of
        ECon c -> tcPat mt $ eApps p (replicate (conArity c) eDummy)          
        _      -> impossible
    EUpdate p isps -> do
      me <- dsUpdate (const eDummy) p isps
      case me of
        Just p' -> tcPat mt p'
        Nothing -> impossible

    EOr ps -> do
      let orFun = ELam noSLoc $ [ eEqn [p] true | p <- ps] ++ [ eEqn [eDummy] (eFalse loc) ]
          true = eTrue loc
      tcPat mt $ EViewPat orFun true

    _ -> error $ "tcPat: not handled " ++ show (getSLoc ae) ++ " " ++ show ae

-- The expected type is for (eApps afn (reverse args))
tcPatAp :: HasCallStack =>
           Expected -> [EPat] -> EPat -> T EPatRet
--tcPatAp mt args afn | trace ("tcPatAp: " ++ show (mt, args, afn)) False = undefined
tcPatAp mt args afn =
  case afn of
    EVar i | isConIdent i -> do
      (con, xpt) <- tLookupV i
      tcPatApCon mt args con xpt

    EQVar con xpt -> tcPatApCon mt args con xpt

    EApp f a -> tcPatAp mt (a:args) f

    EParen e -> tcPatAp mt args e

    _ -> tcError (getSLoc afn) ("Bad pattern " ++ show afn)

tcPatApCon :: Expected -> [EPat] -> EPat -> EType -> T EPatRet
tcPatApCon mt args con xpt = do
  let loc = getSLoc con
      nargs = length args
      checkArity ary =
        if nargs < ary then
          tcError loc "too few arguments"
        else if nargs > ary then
          tcError loc "too many arguments"
        else
          return ()
  case con of
    -- Pattern synonym
    ECon (ConSyn qi n (e, t)) -> do
      checkArity n
      let (_, yes, _) = mkMatchDataTypeConstr (mkPatSynMatch qi) xpt
          vp = EViewPat (EQVar e t) (eApps yes args)
      --traceM ("patsyn " ++ show vp)
      tcPat mt vp

    -- Regular constructor
    _ -> do 
      case xpt of
         -- Sanity check
         EForall _ _ (EForall _ _ _) -> return ()
         _ -> impossibleShow con
      EForall _ avs apt <- tInst' xpt

      (sks, spt) <- shallowSkolemise avs apt
      (df, pf, pt) <-
        case getImplies spt of
          Nothing -> return ([], con, apt)
          Just (ctx, pt') -> do
            di <- newADictIdent loc
            return ([(di, ctx)], EApp con (EVar di), pt')
          
      let ary = arity pf
            where arity (ECon c) = conArity c
                  arity (EApp f _) = arity f - 1  -- deal with dictionary added above
                  arity e = impossibleShow e
      checkArity ary

      let step [] t r = return (t, r)
          step (a:as) t (sk, d, f) = do
            (at, rt) <- unArrow loc t
            (ska, da, a') <- tCheckPat at a
            step as rt (ska ++ sk, da ++ d, EApp f a')
      (tt, (skr, dr, pr)) <- step args pt (sks, df, pf)

      pp <- case mt of
              Check ext -> subsCheck loc pr ext tt
              Infer r   -> do { tSetRefType loc r tt; return pr }
      return (skr, dr, pp)

eTrue :: SLoc -> Expr
eTrue l = EVar $ mkBuiltin l "True"

eFalse :: SLoc -> Expr
eFalse l = EVar $ mkBuiltin l "False"

multCheck :: [Ident] -> T ()
multCheck vs =
  when (anySame vs) $ do
    let v = head vs
    tcError (getSLoc v) $ "Multiply defined: " ++ showIdent v

tcBinds :: forall a . [EBind] -> ([EBind] -> T a) -> T a
tcBinds xbs ta = withFixes [ (i, fx) | Infix fx is <- xbs, i <- is ] $ do
  let
    tmap = M.fromList [ (i, t) | Sign is t <- xbs, i <- is ]
    xs = getBindsVars xbs
  multCheck xs
  xts <- mapM (tcBindVarT tmap) xs
  withExtVals xts $ do
    nbs <- mapM tcBind xbs
    ta nbs

-- Temporarily exten the fixity table
withFixes :: [FixDef] -> T a -> T a
withFixes [] ta = ta
withFixes fixs ta = do
  ft <- gets fixTable
  modify $ \ st -> st{ fixTable = foldr (uncurry M.insert) ft fixs }
  a <- ta
  modify $ \ st -> st{ fixTable = ft }
  return a

tcBindVarT :: HasCallStack => M.Map EType -> Ident -> T (Ident, EType)
tcBindVarT tmap x = do
  case M.lookup x tmap of
    Nothing -> do
      t <- newUVar
      return (x, t)
    Just t -> do
      tt <- withTypeTable $ tCheckTypeTImpl kType t
      return (x, tt)

tcBind :: EBind -> T EBind
tcBind abind =
  case abind of
    Fcn i eqns -> do
      (_, tt) <- tLookupV i
      teqns <- tcEqns False tt eqns
      return $ Fcn i teqns
    PatBind p a -> tcPatBind PatBind p a
    _ -> return abind

tcPatBind :: (EPat -> Expr -> a) -> EPat -> Expr -> T a
tcPatBind con p a = do
  ((sk, ds, ep), tp) <- tInferPat p  -- pattern variables already bound
  -- This is just to complicated.
  when (not (null sk) || not (null ds)) $
    tcError (getSLoc p) "existentials not allowed in pattern binding"
  ea <- tCheckExprAndSolve tp a
  return $ con ep ea


-- Desugar [T] and (T,T,...)
dsType :: EType -> EType
dsType at =
  case at of
    EVar _ -> at
    EApp f a -> EApp (dsType f) (dsType a)
    EOper t ies -> EOper (dsType t) [(i, dsType e) | (i, e) <- ies]
    EListish (LList [t]) -> tApp (tList (getSLoc at)) (dsType t)
    ETuple ts -> tApps (tupleConstr (getSLoc at) (length ts)) (map dsType ts)
    EParen t -> dsType t
    ESign t k -> ESign (dsType t) k
    EForall b iks t -> EForall b iks (dsType t)
    ELit _ (LStr _) -> at
    ELit _ (LInteger _) -> at
    _ -> impossible

tListI :: SLoc -> Ident
tListI loc = mkIdentSLoc loc nameList

tList :: SLoc -> EType
tList = tCon . tListI

tBool :: SLoc -> EType
tBool loc = tConI loc $ boolPrefix ++ "Bool"

showTModule :: forall a . (a -> String) -> TModule a -> String
showTModule sh amdl = "Tmodule " ++ showIdent (tModuleName amdl) ++ "\n" ++ sh (tBindingsOf amdl) ++ "\n"

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

tyVarSubst :: [a] -> EType -> ([IdKind], [(a, EType)])
tyVarSubst tvs ty =
  let usedVars = allVarsExpr ty -- Avoid used type variables
      newVars = take (length tvs) (allBinders \\ usedVars)
      newVarsK = map (\ i -> IdKind i noKind) newVars
      noKind = eDummy
  in  (newVarsK, zipWith (\ tv n -> (tv, EVar n)) tvs newVars)

-- Quantify over the specified type variables.
-- The type should be zonked.
quantify :: [TRef] -> Rho -> T Sigma
quantify [] ty = return ty
quantify tvs ty = do
  let (newVarsK, sub) = tyVarSubst tvs ty
  osubst <- gets uvarSubst
  mapM_ (uncurry setUVar) sub
  ty' <- derefUVar ty
  putUvarSubst osubst  -- reset the setUVar we did above
  return (EForall False newVarsK ty')

allBinders :: [Ident] -- a,b,...,z,a1,a2,...
allBinders = [ mkIdent [x] | x <- ['a' .. 'z'] ] ++
             [ mkIdent ('a' : show i) | i <- [1::Int ..]]

-- Skolemize the given variables
shallowSkolemise :: [IdKind] -> EType -> T ([TyVar], EType)
shallowSkolemise tvs ty = do
  sks <- mapM (newSkolemTyVar . idKindIdent) tvs
  return (sks, subst (zip (map idKindIdent tvs) (map EVar sks)) ty)

skolemise :: HasCallStack =>
             Sigma -> T ([TyVar], Rho)
-- Performs deep skolemisation, returning the
-- skolem constants and the skolemised type.
skolemise (EForall _ tvs ty) = do -- Rule PRPOLY
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

metaTvs :: [EType] -> [TRef]
-- Get the MetaTvs from a type; no duplicates in result
metaTvs tys = foldr go [] tys
  where
    go (EUVar tv) acc
      | elem tv acc = acc
      | otherwise = tv : acc
    go (EVar _) acc = acc
    go (EForall _ _ ty) acc = go ty acc
    go (EApp fun arg) acc = go fun (go arg acc)
    go (ELit _ _) acc = acc
    go _ _ = impossible

{-
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
--  sigma' <- derefUVar sigma
--  tcTrace $ "**** checkSigma: " ++ show expr ++ " :: " ++ show sigma ++ " = " ++ show sigma' ++ " " ++ show skol_tvs
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
-- XXX Is this even right?  It's not part of the paper.
subsCheckRho loc exp1 (EForall _ vs1 t1) (EForall _ vs2 t2) | length vs1 == length vs2 = do
  let sub = [(v1, EVar v2) | (IdKind v1 _, IdKind v2 _) <- zip vs1 vs2]
  unify loc (subst sub t1) t2
  return exp1
subsCheckRho loc exp1 sigma1@(EForall _ _ _) rho2 = do -- Rule SPEC
  (exp1', rho1) <- tInst exp1 sigma1
  subsCheckRho loc exp1' rho1 rho2
subsCheckRho loc exp1 arho1 rho2 | Just _ <- getImplies arho1 = do
  (exp1', rho1) <- tInst exp1 arho1
  subsCheckRho loc exp1' rho1 rho2
subsCheckRho loc exp1 rho1 rho2 | Just (a2, r2) <- getArrow rho2 = do -- Rule FUN
  (a1, r1) <- unArrow loc rho1
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 rho1 rho2 | Just (a1, r1) <- getArrow rho1 = do -- Rule FUN
  (a2,r2) <- unArrow loc rho2
  subsCheckFun loc exp1 a1 r1 a2 r2
subsCheckRho loc exp1 tau1 tau2 = do  -- Rule MONO
--  tcTrace $ "subsCheckRho: MONO " ++ show (tau1, tau2)
  unify loc tau1 tau2 -- Revert to ordinary unification
  return exp1

subsCheckFun :: HasCallStack =>
                SLoc -> Expr -> Sigma -> Rho -> Sigma -> Rho -> T Expr
subsCheckFun loc e1 a1 r1 a2 r2 = do
  _ <- subsCheck loc eCannotHappen a2 a1   -- XXX
  subsCheckRho loc e1 r1 r2

instSigma :: HasCallStack =>
             SLoc -> Expr -> Sigma -> Expected -> T Expr
instSigma loc e1 t1 (Check t2) = do
--  tcTrace ("instSigma: Check " ++ showEType t1 ++ " = " ++ showEType t2)
  subsCheckRho loc e1 t1 t2
instSigma loc e1 t1 (Infer r) = do
  (e1', t1') <- tInst e1 t1
  --tcTrace ("instSigma: Infer " ++ showEType t1 ++ " ==> " ++ showEType t1')
  tSetRefType loc r t1'
  return e1'

eCannotHappen :: Expr
eCannotHappen = --undefined
                EVar $ mkIdent "cannot-happen"

-----

--
-- Pattern synonyms look like
--   pattern P :: forall a1...an . ctxr => forall b1...bm . ctxp => t1 -> ... -> ti -> t
--   pattern P x1...xi <- p where P = e
-- (this type is the canonicalized type, generated by canonPatSynType).
-- The synonym is translated into a builder, a matcher and a type.
-- Each synonym use is replaced by a simple view pattern.
--
-- The builder is simple.  It gets the same name and type as the pattern synonym,
-- and the definition is the one provided in the definition.
--   P :: forall a1...an . ctxr => forall b1...bm . ctxp => t1 -> ... ti -> t
--   P = e
--
-- The matcher needs to account for possible existentials so we get a data type
-- for the match result that can have existentials.
--   data P%T a1...an = forall b1...bm . ctxp => M t1 ... ti
--                    | N
--
-- The matcher itself has the required part of the synonym type, whereas
-- the provided part is in the data type.  The matcher simply matches on the given pattern.
--   P% :: forall a1...an . ctxr => t -> P%T a1...an
--   P% p = M x1...xi
--   P% _ = N
-- So when the synonym P matches the matcher P% will return the M constructor
-- of the P%T type, and then N constructor when there is no match.
--
-- Each use of the pattern synonym
--   P p1...pi
-- is replaced by
--   (P% -> M p1...pi)
--
-- The data type, P%T, is not entered into any symbol tables.
-- The matcher, P%, is in the symbol table, but is not part of the exported symbols.
-- The transformed expression simply carries enough information about the types
-- (using EQVar).  The exported ECon for P has this information.
--

emptyCtx :: EConstraint
emptyCtx = EVar $ tupleConstr noSLoc 0

isEmptyCtx :: EConstraint -> Bool
isEmptyCtx (EVar i) = i == tupleConstr noSLoc 0
isEmptyCtx _ = False

-- Expand a pattern synonym into the builder and matcher definitions.
-- Removes that actual pattern definition
expandPatSyn :: EDef -> T [EDef]
expandPatSyn (Pattern (i, vks) p me) = do
  (_, t) <- tLookup "type signature" i
  (im, qim) <- addPatSynMatch i t
  let (ddata, yes, no) = mkMatchDataTypeConstr qim t
      mexp = fmap (Fcn i) me
      pat = Fcn im [ eEqn [p] match
                   , eEqn [eDummy] no]
      match = eApps yes (map (EVar . idKindIdent) vks)
      dname = case ddata of Data (n, _) _ _ -> n; _ -> impossible
  kvar <- newUVar    -- We don't care about the kind
  withTypeTable $ extValQTop dname kvar
  pure $ maybeToList mexp ++ [pat, ddata]
expandPatSyn d = pure [d]

-- Add the matcher for a pattern synonym to the symbol table.
-- Return the added identifier.
addPatSynMatch :: Ident -> EType -> T (Ident, Ident)
addPatSynMatch i at = do
  mn <- gets moduleName
  let ip = mkPatSynMatch i
      qip = qualIdent mn ip
  extValETop ip (mkPatSynMatchType qip at) (EVar qip)
  return (ip, qip)

mkPatSynMatchType :: Ident -> EType -> EType
mkPatSynMatchType qip at =
  let (vks1, ctx1, _vks2, _ctx2, ty) = splitPatSynType at
      (_ats, rt) = getArrows ty
      pstycon = mkMatchDataTypeName qip
  in  eForall vks1 $ etImplies ctx1 $ rt `tArrow` tApps pstycon (map (EVar . idKindIdent) vks1)

-- Given the (qualified) name of a synonym and its type generate:
-- match-constructor, nomatch-constructor
mkMatchDataTypeConstr :: HasCallStack => Ident -> EType -> (EDef, Expr, Expr)
mkMatchDataTypeConstr qi at =
  let (vks1, _ctx1, vks2, ctx2, ty) = splitPatSynType at
      (ats, _rt) = getArrows ty
      n = length ats
      mi = addIdentSuffix qi "M"
      ni = addIdentSuffix qi "N"
      cti = [ (mi, n + if isEmptyCtx ctx2 then 0 else 1), (ni, 0) ]
      conm = ConData cti mi []
      conn = ConData cti ni []
      tycon = mkMatchDataTypeName qi
      tr = tApps tycon $ map (EVar . idKindIdent) vks1
      tn = EForall True vks1 $ EForall True [] tr
      tm = EForall True vks1 $ EForall True vks2 $ etImplies ctx2 $ foldr tArrow tr ats

      ddata = Data lhs [cm, cn] []
            where lhs = (unQualIdent tycon, vks1)
                  cm = Constr vks2 (if isEmptyCtx ctx2 then [] else [ctx2]) (unQualIdent mi) (Left $ zip (repeat False) ats)
                  cn = Constr []   []                                       (unQualIdent ni) (Left [])

  in  -- trace ("M :: " ++ show tm ++ ",  N :: " ++ show tn) $
      -- trace (showEDefs [ddata]) $
      (ddata, EQVar (ECon conm) tm, EQVar (ECon conn) tn)

mkPatSynMatch :: Ident -> Ident
mkPatSynMatch i = addIdentSuffix i "%"

mkMatchDataTypeName :: Ident -> Ident
mkMatchDataTypeName i = addIdentSuffix i "T"

isMatchDataTypeName :: Ident -> Bool
isMatchDataTypeName = isSuffixOf "%T" . unIdent

-- A pattern synonym always has a type of the form
--  forall vs1 . ctx1 => forall vs2 . ctx2 => ty
--         required             provided
-- The input has forall inserted, but the implicit forall
-- may be in the wrong place.
canonPatSynType :: EType -> T EType
canonPatSynType at = do
  let mkTyp rVks rCtx pVks pCtx ty =
        EForall True rVks $ tImplies rCtx $
        EForall True pVks $ tImplies pCtx ty
      getImplies' :: EType -> (EConstraint, EType)
      getImplies' ty = fromMaybe (emptyCtx, ty) $ getImplies ty

  at' <- expandSyn at
  case at' of
    EForall False vks t0 -> do
      -- Implicit forall, the xs need to be split between required and provided.
      let (reqCtx, t1) = getImplies' t0
          (proCtx, t2) = getImplies' t1
          proVs = freeTyVars [proCtx]
          (proVks, reqVks) = partition ((`elem` proVs) . idKindIdent) vks
--      traceM "%%% implicit"
      pure $ mkTyp reqVks reqCtx proVks proCtx t2

    EForall True reqVks t0 -> do
      -- Explicit forall
      let (reqCtx, t1) = getImplies' t0
          (proVks, t2) = unForall t1
          (proCtx, t3) = getImplies' t2
--      traceM "%%% explicit"
      pure $ mkTyp reqVks reqCtx proVks proCtx t3

    ty -> do
      -- No forall at all.  XXX doesn't work with nullary classes
--      traceM "%%% none"
      pure $ mkTyp [] emptyCtx [] emptyCtx ty

splitPatSynType :: EType -> ([IdKind], EConstraint, [IdKind], EConstraint, EType)
splitPatSynType (EForall _ vks1 t0)
  | Just  (ctx1, EForall _ vks2 t1) <- getImplies t0
  , Just  (ctx2, ty) <- getImplies t1
  = (vks1, ctx1, vks2, ctx2, ty)
splitPatSynType t = impossibleShow t

-----

-- Given a dictionary of a (constraint type), split it up
--  * components of a tupled constraint
--  * superclasses of a constraint
expandDict :: HasCallStack => Expr -> EConstraint -> T [InstDictC]
expandDict edict ct = expandDict' [] [] edict =<< expandSyn ct

expandDict' :: HasCallStack => [IdKind] -> [EConstraint] -> Expr -> EConstraint -> T [InstDictC]
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
        Nothing ->
         -- XXX ~ could be in the symbol table
         if iCls == mkIdent "Primitives.~" then
          return []
         else do
          -- if iCls is a variable it's not in the class table, otherwise it's an error
          when (isConIdent iCls) $
            --impossible
            -- XXX it seems we can get here, e.g., Control.Monad.Fail without Applicative import
            error ("expandDict: " ++ showExprRaw acc)
          return [(edict, vks, ctx, cc, [])]
        Just (ClassInfo iks sups _ _ fds) -> do
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
--  traceM $ "solveLocalConstraints: " ++ show (cs, ds, un)
  putConstraints (un ++ cs)        -- put back unsolved and old constraints
  return (a, ds)

solveAndDefault :: forall a . Bool -> T a -> T (a, [Solved])
solveAndDefault False ta = solveLocalConstraints ta
solveAndDefault True  ta = do
  a <- ta
  ds <- solveConstraints
  cs <- gets constraints
  vs <- getMetaTyVars (map snd cs)    -- These are the type variables that need defaulting
--  tcTrace $ "solveAndDefault: meta=" ++ show vs
  -- XXX may have to iterate this with fundeps
  ds' <- concat <$> mapM defaultOneTyVar vs
  return (a, ds ++ ds')

defaultOneTyVar :: TRef -> T [Solved]
defaultOneTyVar tv = do
--  traceM $ "defaultOneTyVar: " ++ show tv
  old <- get             -- get entire old state
  let cvs = nub [ c | (_, EApp (EVar c) (EUVar tv')) <- constraints old, tv == tv' ]  -- all C v constraints
--  traceM $ "defaultOneTyVar: cvs = " ++ show cvs
  dvs <- getSuperClasses cvs                            -- add superclasses
--  traceM $ "defaultOneTyVar: dvs = " ++ show dvs
  let oneCls c | Just ts <- M.lookup c (defaults old) =
        take 1 $ filter (\ t -> all (\ cc -> soluble cc t) cvs) ts
               | otherwise = []
      soluble c t = fst $ flip tcRun old $ do
        putConstraints [(dummyIdent, EApp (EVar c) t)]  -- Use current (C T) constraint
        _ <- solveConstraints                           -- and solve.
        cs <- gets constraints
        return $ null cs                                -- No constraints left?
      tys = nubBy eqEType $ concatMap oneCls dvs
--  traceM $ "defaultOneTyVar: " ++ show (tv, tys)
  case tys of
    [ty] -> do            -- There is a single type solving everything
      setUVar tv ty
      solveConstraints
    _ -> return []        -- Nothing solved

-- The transitive closure of super-classes.
-- XXX Somewhat duplicated with expandDict
getSuperClasses :: [Ident] -> T [Ident]
getSuperClasses ais = do
  ct <- gets classTable
  let loop done [] = done
      loop done (i:is) | i `elem` done = loop done is
                       | otherwise = i :
        case M.lookup i ct of
          Nothing -> error $ "getSuperClasses: " ++ show i
          Just (ClassInfo _ supers _ _ _) ->
            loop done (concatMap flatten supers ++ is)
      flatten a | (c, ts) <- getApp a =
        case getTupleConstr c of
          Nothing -> [c]
          Just _ -> concatMap flatten ts
  return $ loop [] ais


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
--    tcTrace "------------------------------------------\nsolveConstraints"
    cs' <- mapM (\ (i,t) -> do { t' <- derefUVar t; return (i,t') }) cs
--    tcTrace ("constraints:\n" ++ unlines (map showConstraint cs'))
    (unsolved, solved, improves) <- solveMany cs' [] [] []
    putConstraints unsolved
--    tcTrace ("solved:\n"   ++ unlines [ showIdent i ++ " = "  ++ showExpr  e | (i, e) <- solved ])
--    tcTrace ("unsolved:\n" ++ unlines [ showIdent i ++ " :: " ++ showEType t | (i, t) <- unsolved ])
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
  , ((== mkIdent nameKnownNat),    solveKnownNat)     -- KnownNat 123 constraints
  , ((== mkIdent nameKnownSymbol), solveKnownSymbol)  -- KnownSymbol "abc" constraints
  , ((== mkIdent nameCoercible),   solveCoercible)    -- Coercible a b constraints
  , (const True,                   solveInst)         -- handle constraints with instances
  ]

-- Examine each goal, either solve it (possibly producing new goals) or let it remain unsolved.
solveMany :: [Goal] -> [UGoal] -> [Soln] -> [Improve] -> T ([UGoal], [Soln], [Improve])
solveMany [] uns sol imp = return (uns, sol, imp)
-- Need to handle ct of the form C => T, and forall a . T
solveMany (cns@(di, ct) : cnss) uns sol imp = do
--  tcTrace ("trying " ++ showEType ct)
  let loc = getSLoc di
      !(iCls, cts) = getApp ct
      solver = head [ s | (p, s) <- solvers, p iCls ]
  ads <- gets argDicts
  -- Check if we have an exact match among the arguments dictionaries.
  -- This is important to find tupled dictionaries in recursive calls.
  case [ ai | (ai, act) <- ads, ct `eqEType` act ] of
    ai : _ -> do
      --tcTrace $ "solve with arg " ++ show ct
      solveMany cnss uns ((di, EVar ai) : sol) imp
    [] -> do
      msol <- solver loc iCls cts
      case msol of
        Nothing           -> solveMany        cnss  (cns : uns)            sol         imp
        Just (de, gs, is) -> solveMany (gs ++ cnss)        uns ((di, de) : sol) (is ++ imp)

solveInst :: SolveOne
solveInst loc iCls cts = do
  it <- gets instTable
--  tcTrace ("instances:\n" ++ unlines (map showInstDef (M.toList it)))
  -- XXX The solveGen&co functions are not in the T monad.
  -- But we sometimes need to instantiate type variable, so we use the
  -- hack to pass dowwn a starting uniq.
  -- This ought to be fixed, but is will be less efficient.
  uniq <- do ts <- get; let { u = unique ts }; put ts{ unique = u+100 }; return u   -- make room for many UVars
  case M.lookup iCls it of
    Nothing -> return Nothing   -- no instances, so no chance
    Just (InstInfo atomMap insts fds) -> do
      -- tcTrace $ "solveInst: " ++ showIdent iCls ++ " atomMap size=" ++ show (M.size atomMap)
      case cts of
        [EVar i] -> do
          case M.lookup i atomMap of
            -- If the goal is just (C T) and there is an instance, the solution is simple
            Just e  -> return $ Just (e, [], [])
            -- Not found, but there might be a generic instance
            Nothing -> solveGen uniq (M.null atomMap) fds insts loc iCls cts
        _           -> solveGen uniq (M.null atomMap) fds insts loc iCls cts

-- When matching constraint (C _a) against an instance of the form
-- instance (C b) then we don't want to match if the
-- _a is later instantiated and it turns out we should
-- have matched a (C T) instead.
solveGen :: TRef -> Bool -> [IFunDep] -> [InstDict] -> SolveOne
solveGen uniq noAtoms fds insts loc iCls cts = do
--  tcTrace ("solveGen: " ++ show (iCls, cts))
--  tcTrace ("solveGen: insts=" ++ show insts)
  let matches = getBestMatches $ findMatches uniq noAtoms loc fds insts cts
--  tcTrace ("solveGen: matches allMatches =" ++ showListS show (findMatches uniq noAtoms loc fds insts cts))
--  tcTrace ("solveGen: matches bestMatches=" ++ showListS showMatch matches)
  case matches of
    []              -> return Nothing
    [(de, ctx, is)] ->
      if null ctx then
        return $ Just (de, [], is)
      else do
        d <- newDictIdent loc
--        tcTrace ("constraint " ++ showIdent di ++ " :: " ++ showEType ct ++ "\n" ++
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
-- If either type is a unification variable, just do the unification.
solveTypeEq loc _iCls [t1, t2] | isEUVar t1 || isEUVar t2 = return $ Just (ETuple [], [], [(loc, t1, t2)])
                               | otherwise = do
  eqs <- gets typeEqTable
  --tcTrace ("solveTypeEq eqs=" ++ show eqs)
  case solveEq eqs t1 t2 of
    Nothing -> return Nothing
    Just tts -> do
      let mkEq (u1, u2) = do
            i <- newDictIdent loc
            return (i, mkEqType loc u1 u2)
      ncs <- mapM mkEq tts
      return $ Just (ETuple [], ncs, [])
solveTypeEq _ _ _ = impossible

solveCoercible :: SolveOne
solveCoercible loc _iCls [t1, t2] = do
  st <- gets synTable
  extNewtypeSyns        -- pretend newtypes are type synonyms
  t1' <- expandSyn t1
  t2' <- expandSyn t2
  putSynTable st
  -- walk over the types in parallel,
  -- and generate new Coercible constraints when not equal.
--  traceM $ "solveCoercible: " ++ showExprRaw t1' ++ " and " ++ showExprRaw t2'
  case genCoerce t1' t2' of
    Nothing -> return Nothing
    Just [(u1, u2)] | u1 `eqEType` t1 && u2 `eqEType` t2 -> return Nothing  -- Nothing has improved
    Just tts -> do
      let mkCo (u1, u2) = do
            i <- newDictIdent loc
            return (i, mkCoercible loc u1 u2)
      ncs <- mapM mkCo tts
      return $ Just (ETuple [], ncs, [])
solveCoercible _ _ _ = impossible

genCoerce :: EType -> EType -> Maybe [(EType, EType)]
genCoerce t1 t2 | eqEType t1 t2 = Just []
genCoerce t1@(EUVar _) t2 = Just [(t1, t2)]
genCoerce t1@(EVar _)  t2 = Just [(t1, t2)]
genCoerce t1 t2@(EUVar _) = Just [(t1, t2)]
genCoerce t1 t2@(EVar _)  = Just [(t1, t2)]
genCoerce (EApp f1 a1) (EApp f2 a2) = (++) <$> genCoerce f1 f2 <*> genCoerce a1 a2
genCoerce _ _ = Nothing

-- Pretend newtypes are type synonyms.
-- XXX It's rather inefficient to do this over and over.
extNewtypeSyns :: T ()
extNewtypeSyns = do
  dt <- gets dataTable
  let ext (qi, Newtype (_, vs) (Constr _ _ _c et) _) = do
          -- XXX We should check that the constructor name (_c) is visible.
          -- But this is tricky since we don't know under what qualified name it
          -- it should be visible.
          let t = either (snd . head) (snd . snd . head) et
--          traceM $ "extNewtypeSyns: " ++ showIdent qi ++ show vs ++ " = " ++ showExprRaw t
          extSyn qi (EForall True vs t)  -- extend synonym table
      ext _ = return ()
  mapM_ ext $ M.toList dt
  

isEUVar :: EType -> Bool
isEUVar (EUVar _) = True
isEUVar _ = False

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
findMatches :: TRef -> Bool -> SLoc -> [IFunDep] -> [InstDict] -> [EType] -> [(Int, (Expr, [EConstraint], [Improve]))]
findMatches _ False _ _ _ [EUVar _] = []
findMatches uniq _ loc fds ds its =
 let rrr =
       [ (length s, (de, map (substEUVar s) ctx, imp))
       | (de, ctxts) <- ds
       , let (ctx, ts) = ctxts uniq
       , Just (s, imp) <- [matchTypes loc ts its fds]
       ]
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
substEUVar s (EForall b iks t) = EForall b iks (substEUVar s t)
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
--matchTypesFD _ ts ts' io | trace ("matchTypesFD: " ++ show (ts, ts', io)) False = undefined
matchTypesFD loc ts ts' (ins, outs) = do
  let matchFD :: Bool -> EType -> EType -> Maybe TySubst
      matchFD True  = \ _ _ -> Just []     -- if it's an output, don't match
      matchFD False = matchType []          -- match types for non-outputs
  tms <- sequence $ zipWith3 matchFD outs ts ts'
  tm  <- combineTySubsts tms               -- combine all substitutions
  is  <- combineTySubsts [ s | (True, s) <- zip ins tms]  -- subst from input FDs
  let imp = [ (loc, substEUVar is t, t') | (True, t, t') <- zip3 outs ts ts' ]  -- improvements
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
--    match _ _ (EUVar _) = Nothing
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
-- Always prefer arguments rather than global instances.
getBestMatches :: [(Int, (Expr, [EConstraint], [Improve]))] -> [(Expr, [EConstraint], [Improve])]
getBestMatches [] = []
getBestMatches ams =
  let isDictArg (EVar i) = (adictPrefix ++ uniqIdentSep) `isPrefixOf` unIdent i
      isDictArg _ = True
      !(args, insts) = partition (\ (_, (ei, _, _)) -> isDictArg ei) ams
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
--      tcTrace $ "Cannot satisfy constraint: " ++ unlines (map (\ (i, ii) -> show i ++ ":\n" ++ showInstInfo ii) (M.toList is))
      tcError (getSLoc i) $ "Cannot satisfy constraint: " ++ show t'
                            ++ "\n     fully qualified: " ++ showExprRaw t'

-- Add a type equality constraint.
addEqConstraint :: SLoc -> EType -> EType -> T ()
addEqConstraint loc t1 t2 = do
  d <- newDictIdent loc
  addConstraint d (mkEqType loc t1 t2)

mkEqType :: SLoc -> EType -> EType -> EConstraint
mkEqType loc t1 t2 = eAppI2 (mkIdentSLoc loc nameTypeEq) t1 t2

mkCoercible :: SLoc -> EType -> EType -> EConstraint
mkCoercible loc t1 t2 = eAppI2 (mkIdentSLoc loc nameCoercible) t1 t2

-- Possibly solve a type equality.
solveEq :: TypeEqTable -> EType -> EType -> Maybe [(EType, EType)]
--solveEq eqs t1 t2 | trace ("solveEq: " ++ show (t1,t2) ++ show eqs) False = undefined
solveEq eqs t1 t2 | t1 `eqEType` t2            = Just []
                  | elemBy eqTyTy (t1, t2) eqs = Just []
                  | otherwise =
  case (t1, t2) of
    (EApp f1 a1, EApp f2 a2) -> Just [(f1, f2), (a1, a2)]
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
showSymTab :: SymTab -> String
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

doDeriving :: EDef -> T [EDef]
doDeriving def@(Data    lhs cs ds) = (def:) . concat <$> mapM (deriveHdr lhs  cs) ds
doDeriving def@(Newtype lhs  c ds) = (def:) . concat <$> mapM (deriveHdr lhs [c]) ds
doDeriving def@(Deriving ct)       = (def:) <$> standaloneDeriving ct
doDeriving def                     = return [def]

standaloneDeriving :: EType -> T [EDef]
standaloneDeriving act = do
  (_vks, _ctx, cc) <- splitInst <$> expandSyn act
  dtable <- gets dataTable
--  traceM ("standaloneDeriving 1 " ++ show (ctx, cc))
  (cls, tname) <-
    case getAppM cc of
      Just (c, ts@(_:_)) |
        let t = last ts,
        Just (n, _) <- getAppM t -> return (tApps c (init ts), n)
      _ -> tcError (getSLoc act) "malformed standalone deriving"
--  traceM ("standaloneDeriving 2 " ++ show (act, cls, tname))
  (lhs, cs) <-
    case M.lookup tname dtable of
      Just (Newtype l  c _) -> return (l, [c])
      Just (Data    l xs _) -> return (l, xs)
      _ -> tcError (getSLoc act) ("not data/newtype " ++ showIdent tname)
  -- We want 'instance ctx => cls ty'
  deriveNoHdr act lhs cs cls
