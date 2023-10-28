-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
module MicroHs.TypeCheck(
  typeCheck,
  TModule(..), showTModule,
  impossible,
  ) where
import Prelude --Xhiding(showList)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
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
  [ValueExport]   -- exported values (including from T(..))
  a               -- bindings
  --Xderiving (Show)

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

-- Symbol table entry for symbol i.
data Entry = Entry
  Expr             -- convert (EVar i) to this expression; sometimes just (EVar i)
  EType            -- type/kind of identifier
  --Xderiving(Show)

entryType :: Entry -> EType
entryType (Entry _ t) = t

type ValueTable = SymTab Entry     -- type of value identifiers, used during type checking values
type TypeTable  = SymTab Entry     -- kind of type  identifiers, used during kind checking types
type KindTable  = SymTab Entry     -- sort of kind  identifiers, used during sort checking kinds
type SynTable   = M.Map EType      -- body of type synonyms
type FixTable   = M.Map Fixity     -- precedence and associativity of operators
type AssocTable = M.Map [Ident]    -- maps a type identifier to its associated construcors/selectors/methods

type Sigma = EType
--type Tau   = EType
type Rho   = EType
type TyVar = Ident

typeCheck :: forall a . [(ImportSpec, TModule a)] -> EModule -> TModule [EDef]
typeCheck aimps (EModule mn exps defs) =
--  trace (show amdl) $
  let
    imps = map filterImports aimps
    (fs, ts, ss, vs, as) = mkTables imps
  in case tcRun (tcDefs defs) (initTC mn fs ts ss vs as) of
       (tds, tcs) ->
         let
           thisMdl = (mn, mkTModule tds tcs)
           impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ m mm _, tm) <- imps]
           impMap = M.fromList [(i, m) | (i, m) <- thisMdl : impMdls]
           (texps, vexps) =
             unzip $ map (getTVExps impMap (typeTable tcs) (valueTable tcs) (assocTable tcs)) exps
           fexps = [ fe | TModule _ fe _ _ _ _ <- M.elems impMap ]
           sexps = [ se | TModule _ _ _ se _ _ <- M.elems impMap ]
         in  tModule mn (nubBy (eqIdent `on` fst) (concat fexps)) (concat texps) (concat sexps) (concat vexps) tds

-- A hack to force evaluation of errors.
-- This should be redone to all happen in the T monad.
tModule :: IdentModule -> [FixDef] -> [TypeExport] -> [SynDef] -> [ValueExport] -> [EDef] ->
           TModule [EDef]
tModule mn fs ts ss vs ds = seqL ts `seq` seqL vs `seq` TModule mn fs ts ss vs ds
  where
    seqL :: forall a . [a] -> ()
    seqL [] = ()
    seqL (x:xs) = x `seq` seqL xs

filterImports :: forall a . (ImportSpec, TModule a) -> (ImportSpec, TModule a)
filterImports it@(ImportSpec _ _ _ Nothing, _) = it
filterImports (imp@(ImportSpec _ _ _ (Just (hide, is))), TModule mn fx ts ss vs a) =
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
    (imp, TModule mn fx ts' ss vs' a)

-- Type and value exports
getTVExps :: forall a . M.Map (TModule a) -> TypeTable -> ValueTable -> AssocTable -> ExportItem ->
           ([TypeExport], [ValueExport])
getTVExps impMap _ _ _ (ExpModule m) =
  case M.lookup m impMap of
    Just (TModule _ _ te _ ve _) -> (te, ve)
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

expLookup :: Ident -> SymTab Entry -> Entry
expLookup i m = either (errorMessage (getSLocIdent i)) id $ stLookup "export" i m

tyQIdent :: Entry -> Ident
tyQIdent (Entry (EVar qi) _) = qi
tyQIdent _ = error "tyQIdent"

eVarI :: SLoc -> String -> Expr
eVarI loc = EVar . mkIdentSLoc loc

--tcExpErr :: forall a . Ident -> T a
--tcExpErr i = tcError (getSLocIdent i) $ ": export undefined " ++ showIdent i

expErr :: forall a . Ident -> a
expErr i = errorMessage (getSLocIdent i) $ ": export undefined " ++ showIdent i

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

    -- Find the Entry for a type.
    tentry i =
      case stLookup "" (qualIdent mn i) tt of
        Right e -> e
        _       -> impossible
    -- Find all value Entry for names associated with a type.
    assoc i = getAssocs vt at (qualIdent mn i)

    -- All top level values possible to export.
    ves = [ ValueExport i (Entry (EVar (qualIdent mn i)) ts) | Sign i ts <- tds ]

    -- All top level types possible to export.
    tes =
      [ TypeExport i (tentry i) (assoc i) | Data    (i, _) _ <- tds ] ++
      [ TypeExport i (tentry i) (assoc i) | Newtype (i, _) _ <- tds ] ++
      [ TypeExport i (tentry i) []        | Type    (i, _) _ <- tds ]

    -- All type synonym definitions.
    ses = [ (qualIdent mn i, EForall vs t) | Type (i, vs) t  <- tds ]

    -- All fixity declaration.
    fes = [ (qualIdent mn i, fx) | Infix fx is <- tds, i <- is ]
  in  TModule mn fes tes ses ves impossible

-- Find all value Entry for names associated with a type.
getAssocs :: ValueTable -> AssocTable -> Ident -> [ValueExport]
getAssocs vt at ai =
  let qis = fromMaybe [] $ M.lookup ai at
      val qi = case stLookup "" qi vt of
                 Right e -> e
                 _       -> impossible
  in  map (\ qi -> ValueExport (unQualIdent qi) (val qi)) qis

mkTables :: forall a . [(ImportSpec, TModule a)] -> (FixTable, TypeTable, SynTable, ValueTable, AssocTable)
mkTables mdls =
  let
    qns (ImportSpec q _ mas _) mn i =
      let
        m = fromMaybe mn mas
      in  if q then [qualIdent m i] else [i, qualIdent m i]
    allValues :: ValueTable
    allValues =
      let
        syms (is, TModule mn _ tes _ ves _) =
          [ (v, [e]) | ValueExport i e    <- ves,                        v <- qns is mn i ] ++
          [ (v, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, v <- qns is mn i ]
      in  stFromListWith (unionBy eqEntry) $ concatMap syms mdls
    allSyns =
      let
        syns (_, TModule _ _ _ ses _ _) = ses
      in  M.fromList (concatMap syns mdls)
    allTypes :: TypeTable
    allTypes =
      let
        types (is, TModule mn _ tes _ _ _) = [ (v, [e]) | TypeExport i e _ <- tes, v <- qns is mn i ]
      in stFromListWith (unionBy eqEntry) $ concatMap types mdls
    allFixes =
      let
        fixes (_, TModule _ fes _ _ _ _) = fes
      in M.fromList (concatMap fixes mdls)
    allAssocs :: AssocTable
    allAssocs =
      let
        assocs (ImportSpec _ _ mas _, TModule mn _ tes _ _ _) =
          let
            m = fromMaybe mn mas
          in  [ (qualIdent m i, [qualIdent m a | ValueExport a _ <- cs]) | TypeExport i _ cs <- tes ]
      in  M.fromList $ concatMap assocs mdls
  in  (allFixes, allTypes, allSyns, allValues, allAssocs)

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

--------------------------

type Typed a = (a, EType)

data TCState = TC IdentModule Int FixTable TypeTable SynTable ValueTable AssocTable (IM.IntMap EType) TCMode
  --Xderiving (Show)

data TCMode = TCExpr | TCPat | TCType
  --Xderiving (Show)

typeTable :: TCState -> TypeTable
typeTable (TC _ _ _ tt _ _ _ _ _) = tt

valueTable :: TCState -> ValueTable
valueTable (TC _ _ _ _ _ vt _ _ _) = vt

synTable :: TCState -> SynTable
synTable (TC _ _ _ _ st _ _ _ _) = st

fixTable :: TCState -> FixTable
fixTable (TC _ _ ft _ _ _ _ _ _) = ft

assocTable :: TCState -> AssocTable
assocTable (TC _ _ _ _ _ _ ast _ _) = ast

uvarSubst :: TCState -> IM.IntMap EType
uvarSubst (TC _ _ _ _ _ _ _ sub _) = sub

moduleName :: TCState -> IdentModule
moduleName (TC mn _ _ _ _ _ _ _ _) = mn

tcMode :: TCState -> TCMode
tcMode (TC _ _ _ _ _ _ _ _ m) = m

putValueTable :: ValueTable -> T ()
putValueTable venv = T.do
  TC mn n fx tenv senv _ ast sub m <- get
  put (TC mn n fx tenv senv venv ast sub m)

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = T.do
  TC mn n fx _ senv venv ast sub m <- get
  put (TC mn n fx tenv senv venv ast sub m)

putSynTable :: SynTable -> T ()
putSynTable senv = T.do
  TC mn n fx tenv _ venv ast sub m <- get
  put (TC mn n fx tenv senv venv ast sub m)

putUvarSubst :: IM.IntMap EType -> T ()
putUvarSubst sub = T.do
  TC mn n fx tenv senv venv ast _ m <- get
  put (TC mn n fx tenv senv venv ast sub m)

putTCMode :: TCMode -> T ()
putTCMode m = T.do
  TC mn n fx tenv senv venv ast sub _ <- get
  put (TC mn n fx tenv senv venv ast sub m)

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
  TC mn n fx tt st vt ast sub m <- get
  put (TC mn n fx primKindTable M.empty tt ast sub m)
  a <- ta
  TC mnr nr _ _ _ ttr astr subr mr <- get
  put (TC mnr nr fx ttr st vt astr subr mr)
  T.return a

addAssocTable :: Ident -> [Ident] -> T ()
addAssocTable i is = T.do
  TC mn n fx tt st vt ast sub m <- get
  put $ TC mn n fx tt st vt (M.insert i is ast) sub m

initTC :: IdentModule -> FixTable -> TypeTable -> SynTable -> ValueTable -> AssocTable -> TCState
initTC mn fs ts ss vs as =
--  trace ("initTC " ++ show (ts, vs)) $
  let
    xts = foldr (uncurry stInsert) ts primTypes
    xvs = foldr (uncurry stInsert) vs primValues
  in TC mn 1 fs xts ss xvs as IM.empty TCExpr

kTypeS :: EType
kTypeS = kType

kTypeTypeS :: EType
kTypeTypeS = kArrow kType kType

kTypeTypeTypeS :: EType
kTypeTypeTypeS = kArrow kType $ kArrow kType kType

builtinLoc :: SLoc
builtinLoc = SLoc "builtin" 0 0

mkIdentB :: String -> Ident
mkIdentB = mkIdentSLoc builtinLoc

primKindTable :: KindTable
primKindTable =
  let
    entry i = Entry (EVar (mkIdentB i))
  in stFromList [
       -- The kinds are wired in (for now)
       (mkIdentB "Primitives.Type", [entry "Primitives.Type" kTypeS]),
       (mkIdentB "Type",            [entry "Primitives.Type" kTypeS]),
       (mkIdentB "Primitives.->",   [entry "Primitives.->"   kTypeTypeTypeS]),
       (mkIdentB "->",              [entry "Primitives.->"   kTypeTypeTypeS])
       ]

primTypes :: [(Ident, [Entry])]
primTypes =
  let
    entry i = Entry (EVar (mkIdentB i))
    tuple n =
      let
        i = tupleConstr builtinLoc n
      in  (i, [entry (unIdent i) $ foldr kArrow kType (replicate n kType)])
  in  
      [
       -- The function arrow is bothersome to define in Primtives, so keep it here.
       (mkIdentB "->",           [entry "Primitives.->"       kTypeTypeTypeS]),
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

{-
getTuple :: Int -> EType -> Maybe [EType]
getTuple n t = loop t []
  where loop (EVar i) r | isTupleConstr n i && length r == n = Just (reverse r)
        loop (EApp f a) r = loop f (a:r)
        loop _ _ = Nothing
-}

setUVar :: TRef -> EType -> T ()
setUVar i t = T.do
  TC mn n fx tenv senv venv ast sub m <- get
  put (TC mn n fx tenv senv venv ast (IM.insert i t sub) m)

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
  tcError loc $ "Cannot unify " ++ showExpr t1 ++ " and " ++ showExpr t2 ++ "\n"

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
    tcError loc $ "Cyclic type"
   else
    setUVar r1 t2

{-
unify :: --XHasCallStack =>
         SLoc -> EType -> EType -> T ()
unify loc a b = T.do
--  traceM ("unify1 " ++ showExpr a ++ " = " ++ showExpr b)
  aa <- expandType a
  bb <- expandType b
--  traceM ("unify2 " ++ showExpr aa ++ " = " ++ showExpr bb)
  unifyR loc aa bb

-- XXX should do occur check
unifyR :: --XHasCallStack =>
          SLoc -> EType -> EType -> T ()
unifyR loc a b = T.do
  let
    bad = tcError loc $ "Cannot unify " ++ showExpr a ++ " and " ++ showExpr b ++ "\n"
  case a of
    EVar ia ->
      case b of
        EVar ib  -> if eqIdent ia ib then T.return () else bad
        EApp _ _ -> bad
        EUVar i  -> addUVar i a
        _        -> impossible
    EApp fa xa ->
      case b of
        EVar _     -> bad
        EApp fb xb -> T.do { unify loc fa fb; unify loc xa xb }
        EUVar i    -> addUVar i a
        _          ->
          --Xtrace ("impossible unify 1 " ++ showExpr a ++ " = " ++ showExpr b) $
          impossible
    EUVar i -> addUVar i b
    _ -> --Xtrace ("impossible unify 2 " ++ showExpr a ++ " = " ++ showExpr b) $
         impossible
-}

{-
unMType :: Expected -> T EType
unMType mt =
  case mt of
    Infer r -> T.do
      t <- newUVar
      tSetRefType r t
      T.return t
    Check t -> T.return t
-}

-- Reset type variable and unification map
tcReset :: T ()
tcReset = T.do
  TC mn u fx tenv senv venv ast _ m <- get
  put (TC mn u fx tenv senv venv ast IM.empty m)

newUVar :: T EType
newUVar = EUVar <$> newUniq

type TRef = Int

newUniq :: T TRef
newUniq = T.do
  TC mn n fx tenv senv venv ast sub m <- get
  put (TC mn (n+1) fx tenv senv venv ast sub m)
  T.return n

tLookupInst :: --XHasCallStack =>
               String -> Ident -> T (Expr, EType)
tLookupInst msg i = T.do
  (e, s) <- tLookup msg i
--  traceM ("lookup " ++ show (i, s))
  t <- tInst s
  T.return (e, t)

tLookup :: --XHasCallStack =>
           String -> Ident -> T (Expr, EType)
tLookup msg i = T.do
  env <- gets valueTable
  case stLookup msg i env of
    Right (Entry e s) -> T.return (setSLocExpr (getSLocIdent i) e, s)
    Left            e -> tcError (getSLocIdent i) e

tInst :: EType -> T EType
tInst as =
  case as of
    EForall vks t ->
      if null vks then T.return t
      else T.do
        let vs = map idKindIdent vks
        us <- T.mapM (const newUVar) vks
        T.return (subst (zip vs us) t)
    t -> T.return t

extValE :: --XHasCallStack =>
           Ident -> EType -> Expr -> T ()
extValE i t e = T.do
  venv <- gets valueTable
  putValueTable (stInsert i [Entry e t] venv)

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
  putTypeTable (stInsert i [Entry (EVar i) t] tenv)

extTyps :: [(Ident, EType)] -> T ()
extTyps = T.mapM_ (uncurry extTyp)

extSyn :: Ident -> EType -> T ()
extSyn i t = T.do
  senv <- gets synTable
  putSynTable (M.insert i t senv)

extFix :: Ident -> Fixity -> T ()
extFix i fx = T.do
  TC mn n fenv tenv senv venv ast sub m <- get
  put $ TC mn n (M.insert i fx fenv) tenv senv venv ast sub m
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
  T.mapM tcDefType dsk

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
    _                    -> T.return adef

-- Check&rename the given kinds, apply reconstruction at the end
withVks :: forall a . [IdKind] -> EKind -> ([IdKind] -> EKind -> T a) -> T a
withVks vks kr fun = T.do
  (nvks, nkr) <-
    withTypeTable $ T.do
      let
        loop r [] = T.do
          kkr <- tcInferTypeT kr
          T.return (reverse r, kkr)
        loop r (IdKind i k : iks) = T.do
          kk <- tcInferTypeT k
          withExtVal i kk $ loop (IdKind i kk : r) iks
      loop [] vks
  fun nvks nkr

addTypeKind :: EDef -> T ()
addTypeKind adef = T.do
  let
    addAssoc i is = T.do
      mn <- gets moduleName
      addAssocTable (qualIdent mn i) (map (qualIdent mn) is)
    assocData (Constr c (Left _)) = [c]
    assocData (Constr c (Right its)) = c : map fst its
  case adef of
    Data    lhs@(i, _) cs   -> T.do
      addLHSKind lhs kType
      addAssoc i (nubBy eqIdent $ concatMap assocData cs)
    Newtype lhs@(i, _) c -> T.do
      addLHSKind lhs kType
      addAssoc i (assocData c)
    Type    lhs t   -> addLHSKind lhs (getTypeKind t)
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

tcDefType :: EDef -> T EDef
tcDefType d = T.do
  tcReset
  case d of
    Data    lhs cs -> Data    lhs   <$> withVars (snd lhs) (T.mapM tcConstr cs)
    Newtype lhs c  -> Newtype lhs   <$> withVars (snd lhs) (tcConstr c)
    Type    lhs  t -> Type    lhs   <$> withVars (snd lhs) (tcInferTypeT t)
    Sign    i    t -> (Sign    i  ) <$> tcTypeT (Check kType) t
    ForImp  ie i t -> (ForImp ie i) <$> tcTypeT (Check kType) t
    _ -> T.return d

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
    _ -> T.return ()

unForall :: EType -> ([IdKind], EType)
unForall (EForall iks t) = (iks, t)
unForall t = ([], t)

tcDefValue :: --XHasCallStack =>
              EDef -> T EDef
tcDefValue adef =
  case adef of
    Fcn i eqns -> T.do
--      traceM $ "tcDefValue: " ++ show i -- ++ " = " ++ showExpr rhs
      (_, tt) <- tLookup "no type signature" i
      let (iks, tfn) = unForall tt
      mn <- gets moduleName
      teqns <- withExtTyps iks $ tcEqns tfn eqns
--      traceM (showEDefs [Fcn i eqns, Fcn i teqns])
      T.return $ Fcn (qualIdent mn i) teqns
    ForImp ie i t -> T.do
      mn <- gets moduleName
      T.return (ForImp ie (qualIdent mn i) t)
    _ -> T.return adef

tcInferTypeT :: EType -> T EType
tcInferTypeT t = fst <$> tInfer tcTypeT t

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
tCheckExpr = tCheck tcExpr

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
                (p, pt) <- tLookupInst "constructor" i
                -- We will only have an expected type for a non-nullary constructor
                case mt of
                  Check ext -> subsCheck loc ext pt
                  Infer r   -> tSetRefType r pt
                T.return p

              | otherwise -> T.do
                -- All pattern variables are in the environment as
                -- type references.  Assign the reference the given type.
                ext <- tGetExpTypeSet mt
                (p, t) <- tLookup "IMPOSSIBLE" i
                case t of
                  EUVar r -> tSetRefType r ext
                  _ -> impossible
                T.return p
          
        _ -> T.do
          -- Type checking an expression (or type)
          T.when (isDummyIdent i) impossible
          (e, t) <- tLookup "variable" i
          -- Variables bound in patterns start with an (EUVar ref) type,
          -- which can be instantiated to a polytype.
          -- Dereference such a ref.
          t' <-
            case t of
              EUVar r -> T.fmap (fromMaybe t) (getUVar r)
              _ -> T.return t
--          traceM ("EVar " ++ showIdent i ++ " :: " ++ showExpr t ++ " = " ++ showExpr t')
          instSigma loc t' mt
          T.return e

    EApp f a -> T.do
      (f', ft) <- tInferExpr f
      (at, rt) <- unArrow loc ft
      tcm <- gets tcMode
      case tcm of
        TCPat -> T.do
          a' <- tCheckExpr at a
          instPatSigma loc rt mt
          T.return (EApp f' a')
        _ -> T.do
          a' <- checkSigma a at
          instSigma loc rt mt
          T.return (EApp f' a')

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
            _ -> tcError loc $ "bad do "
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
          (e2', t1) <- tInferExpr e2
          (e3', t2) <- tInferExpr e3
          subsCheck loc t1 t2
          subsCheck loc t2 t1
          tSetRefType ref t1
          T.return (EIf e1' e2' e3')  

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
          instSigma loc t' mt
          checkSigma e t'
    EAt i e -> T.do
      (_, ti) <- tLookup "IMPOSSIBLE" i
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
  let lit t = instSigma loc t mt
  case l of
    LInt _  -> lit (tConI loc "Primitives.Int")
    LDouble _ -> lit (tConI loc "Primitives.Double")
    LChar _ -> lit (tConI loc "Primitives.Char")
    LStr _  -> lit (tApp (tConI loc "Data.List.[]") (tConI loc "Primitives.Char"))
    LPrim _ -> newUVar T.>>= lit  -- pretend it is anything
    LForImp _ -> impossible
  T.return (ELit loc l)

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
        errorMessage (getSLocExpr ox) "Ambiguous operator expression"
       else if px < py || eqAssoc ax AssocLeft && px == py then
        doOp es oy os iies
       else
        calc (e:es) (oo : oos) ies
    calc es [] ((o, e) : ies) =
      calc (e:es) [o] ies
    calc _ _ _ = impossible

    opfix :: FixTable -> (Ident, Expr) -> T ((Expr, Fixity), Expr)
    opfix fixs (i, e) = T.do
      (ei, _) <- tLookup "operator" i
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
tcEqns t eqns = T.mapM (tcEqn t) eqns

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


instPatSigma :: --XHasCallStack =>
                 SLoc -> Sigma -> Expected -> T ()
instPatSigma _   pt (Infer r) = tSetRefType r pt
instPatSigma loc pt (Check t) = subsCheck loc t pt

subsCheck :: --XHasCallStack =>
              SLoc -> Sigma -> Sigma -> T ()
-- (subsCheck args off exp) checks that
-- 'off' is at least as polymorphic as 'args -> exp'
subsCheck loc sigma1 sigma2 = T.do -- Rule DEEP-SKOL
  (skol_tvs, rho2) <- skolemise sigma2
  subsCheckRho loc sigma1 rho2
  esc_tvs <- getFreeTyVars [sigma1,sigma2]
  let bad_tvs = filter (\ i -> elemBy eqIdent i esc_tvs) skol_tvs
  T.when (not (null bad_tvs)) $
    tcError loc "Subsumption check failed"

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
      (_, tt) <- tLookup "impossible!" i
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
    TModule mn _ _ _ _ a -> "Tmodule " ++ showIdent mn ++ "\n" ++ sh a ++ "\n"

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
getEnvTypes = gets (map entryType . concat . stElems . valueTable)

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
      | elemBy eqInt tv acc = acc
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
      tcError (getSLocExpr expr) "Type not polymorphic enough"
    T.return expr'

subsCheckRho :: SLoc -> Sigma -> Rho -> T ()
subsCheckRho loc sigma1@(EForall _ _) rho2 = T.do -- Rule SPEC
  rho1 <- tInst sigma1
  subsCheckRho loc rho1 rho2
subsCheckRho loc rho1 rho2 | Just (a2, r2) <- getArrow rho2 = T.do -- Rule FUN
  (a1, r1) <- unArrow loc rho1
  subsCheckFun loc a1 r1 a2 r2
subsCheckRho loc rho1 rho2 | Just (a1, r1) <- getArrow rho1 = T.do -- Rule FUN
  (a2,r2) <- unArrow loc rho2
  subsCheckFun loc a1 r1 a2 r2
subsCheckRho loc tau1 tau2 -- Rule MONO
  = unify loc tau1 tau2 -- Revert to ordinary unification

subsCheckFun :: SLoc -> Sigma -> Rho -> Sigma -> Rho -> T ()
subsCheckFun loc a1 r1 a2 r2 = T.do
  subsCheck loc a2 a1
  subsCheckRho loc r1 r2

instSigma :: SLoc -> Sigma -> Expected -> T ()
instSigma loc t1 (Check t2) = subsCheckRho loc t1 t2
instSigma _   t1 (Infer r) = T.do
  t1' <- tInst t1
  tSetRefType r t1'

---------------------

data SymTab a = SymTab (M.Map [a]) -- [(Ident, a)]
  --Xderiving(Show)
  
stLookup :: forall a . String -> Ident -> SymTab a -> Either String a
stLookup msg i (SymTab genv) =
  case M.lookup i genv of
    Just [e] -> Right e
    Just _   -> Left $ "ambiguous " ++ msg ++ ": " ++ showIdent i
    Nothing  -> Left $ "undefined " ++ msg ++ ": " ++ showIdent i
                       -- ++ "\n" ++ show env ;

stFromListWith :: forall a . ([a] -> [a] -> [a]) -> [(Ident, [a])] -> SymTab a
stFromListWith comb ias = SymTab (M.fromListWith comb ias)

stFromList :: forall a . [(Ident, [a])] -> SymTab a
stFromList ias = SymTab (M.fromList ias)

stElems :: forall a . SymTab a -> [[a]]
stElems (SymTab genv) = M.elems genv

stInsert :: forall a . Ident -> [a] -> SymTab a -> SymTab a
stInsert i as (SymTab genv) = SymTab (M.insert i as genv)
