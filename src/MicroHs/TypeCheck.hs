-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
module MicroHs.TypeCheck(
  typeCheck,
  TModule(..), showTModule,
  impossible
  ) where
import Prelude
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

data TModule a = TModule IdentModule [FixDef] [TypeExport] [SynDef] [ValueExport] a
  --Xderiving (Show)

data TypeExport = TypeExport Ident Entry [ValueExport]
  --Xderiving (Show)

data ValueExport = ValueExport Ident Entry
  --Xderiving (Show)

type FixDef = (Ident, Fixity)
type SynDef = (Ident, ETypeScheme)

data Entry = Entry Expr ETypeScheme
  --Xderiving(Show)

type ValueTable = M.Map [Entry]
type TypeTable  = M.Map [Entry]
type KindTable  = M.Map [Entry]
type SynTable   = M.Map ETypeScheme
type FixTable   = M.Map Fixity

typeCheck :: forall a . [(ImportSpec, TModule a)] -> EModule -> TModule [EDef]
typeCheck aimps (EModule mn exps defs) =
--  trace (show amdl) $
  let
    imps = map filterImports aimps
    (fs, ts, ss, vs) = mkTables imps
  in case tcRun (tcDefs defs) (initTC mn fs ts ss vs) of
       (tds, tcs) ->
         let
           thisMdl = (mn, mkTModule mn tds impossible)
           impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ m mm _, tm) <- imps]
           impMap = M.fromList [(i, m) | (i, m) <- thisMdl : impMdls]
           (texps, sexps, vexps) =
             unzip3 $ map (getTVExps impMap (typeTable tcs) (synTable tcs) (valueTable tcs)) exps
{-
         in  TModule mn [] (concat texps) (concat sexps) (concat vexps) tds
           (texps, vexps) =
             unzip $ map (getTVExps impMap (typeTable tcs) (valueTable tcs)) exps
           (fexps, sexps) = unzip $ getFSExps impMap
-}
           fexps = [ fe | TModule _ fe _ _ _ _ <- M.elems impMap ]
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
getTVExps :: forall a . M.Map (TModule a) -> TypeTable -> SynTable -> ValueTable -> ExportItem ->
           ([TypeExport], [SynDef], [ValueExport])
getTVExps impMap _ _ _ (ExpModule m) =
  case M.lookup m impMap of
    Just (TModule _ _ te se ve _) -> (te, se, ve)
--    Just (TModule _ _ te _ ve _) -> (te, ve)
    _ -> expErr m
getTVExps _ tys _ vals (ExpTypeCon i) =
  let
    e = expLookup i tys
    qi = tyQIdent e
  in ([TypeExport i e $ constrsOf qi (M.toList vals)], [], [])
getTVExps _ tys syns _ (ExpType i) =
  let
    e = expLookup i tys
    qi = tyQIdent e
    se = case M.lookup qi syns of
           Nothing -> []
           Just ts -> [(qi, ts)]
  in ([TypeExport i e []], se, [])
--  in ([TypeExport i e []], [])
getTVExps _ _ _ vals (ExpValue i) =
    ([], [], [ValueExport i (expLookup i vals)])

-- Export all fixities and synonyms.
-- The synonyms might be needed, and the fixities are harmless
--getFSExps :: forall a . M.Map (TModule a) -> [([FixDef], [SynDef])]
--getFSExps impMap = [ (fe, se) | TModule _ fe _ se _ _ <- M.elems impMap ]

expLookup :: Ident -> M.Map [Entry] -> Entry
expLookup i m =
  case M.lookup i m of
    Just [e] -> e
    Just _ -> errorMessage (getSLocIdent i) $ ": Ambiguous export " ++ showIdent i
    Nothing -> expErr i

tyQIdent :: Entry -> Ident
tyQIdent (Entry (EVar qi) _) = qi
tyQIdent _ = error "tyQIdent"

constrsOf :: Ident -> [(Ident, [Entry])] -> [ValueExport]
constrsOf qi ies =
  [ ValueExport i e | (i, es) <- ies, e@(Entry (ECon _) t) <- es, eqIdent (retTyCon t) qi ]

retTyCon :: EType -> Ident
retTyCon (EForall _ t) = retTyCon t
retTyCon t =
  case getArrow t of
    Nothing -> getAppCon t
    Just (_, a) -> retTyCon a

getAppCon :: EType -> Ident
getAppCon (EVar i) = i
getAppCon (EApp f _) = getAppCon f
getAppCon _ = error "getAppCon"

eVarI :: SLoc -> String -> Expr
eVarI loc = EVar . mkIdentSLoc loc

--tcExpErr :: forall a . Ident -> T a
--tcExpErr i = tcError (getSLocIdent i) $ ": export undefined " ++ showIdent i

expErr :: forall a . Ident -> a
expErr i = errorMessage (getSLocIdent i) $ ": export undefined " ++ showIdent i

mkTModule :: forall a . IdentModule -> [EDef] -> a -> TModule a
mkTModule mn tds a =
  let
    con ci it vks (ic, ts) =
      let
        e = ECon $ ConData ci (qualIdent mn ic)
      in ValueExport ic $ Entry e (EForall vks (foldr tArrow (tApps (qualIdent mn it) (map tVarK vks)) ts))
    cons i vks cs =
      let
        ci = [ (qualIdent mn c, length ts) | (c, ts) <- cs ]
      in map (con ci i vks) cs
    conn it vks ic t =
      let
        e = ECon $ ConNew (qualIdent mn ic)
      in [ValueExport ic $ Entry e (EForall vks (tArrow t (tApps (qualIdent mn it) (map tVarK vks))))]
    tentry i vks kret = Entry (EVar (qualIdent mn i)) (lhsKind vks kret)
    ves = [ ValueExport i (Entry (EVar (qualIdent mn i)) ts) | Sign i ts <- tds ]
    tes =
      [ TypeExport i (tentry i vks kType) (cons i vks cs)  | Data    (i, vks) cs  <- tds ] ++
      [ TypeExport i (tentry i vks kType) (conn i vks c t) | Newtype (i, vks) c t <- tds ] ++
      [ TypeExport i (tentry i vks kType) []               | Type    (i, vks) _   <- tds ]   -- XXX kType is wrong
    ses = [ (qualIdent mn i, EForall vs t) | Type (i, vs) t  <- tds ]
    fes = [ (qualIdent mn i, fx) | Infix fx is <- tds, i <- is ]
  in  TModule mn fes tes ses ves a

mkTables :: forall a . [(ImportSpec, TModule a)] -> (FixTable, TypeTable, SynTable, ValueTable)
mkTables mdls =
  let
    qns aisp mn i =
      case aisp of
        ImportSpec q _ mas _ ->
          let
            m = fromMaybe mn mas
          in  if q then [qualIdent m i] else [i, qualIdent m i]
    allValues :: ValueTable
    allValues =
      let
        syms arg =
          case arg of
            (is, TModule mn _ tes _ ves _) ->
              [ (v, [e]) | ValueExport i e    <- ves,                        v <- qns is mn i ] ++
              [ (v, [e]) | TypeExport  _ _ cs <- tes, ValueExport i e <- cs, v <- qns is mn i ]
      in  M.fromListWith (unionBy eqEntry) $ concatMap syms mdls
    allSyns =
      let
        syns arg =
          case arg of
            (_, TModule _ _ _ ses _ _) -> [ (i, x) | (i, x) <- ses ]
      in  M.fromList (concatMap syns mdls)
    allTypes :: TypeTable
    allTypes =
      let
        types arg =
          case arg of
            (is, TModule mn _ tes _ _ _) -> [ (v, [e]) | TypeExport i e _ <- tes, v <- qns is mn i ]
      in M.fromListWith (unionBy eqEntry) $ concatMap types mdls
    allFixes =
      let
        fixes (_, TModule _ fes _ _ _ _) = fes
      in M.fromList (concatMap fixes mdls)
  in  (allFixes, allTypes, allSyns, allValues)

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

data TCState = TC IdentModule Int FixTable TypeTable SynTable ValueTable (IM.IntMap EType)
  --Xderiving (Show)

typeTable :: TCState -> TypeTable
typeTable (TC _ _ _ tt _ _ _) = tt

valueTable :: TCState -> ValueTable
valueTable (TC _ _ _ _ _ vt _) = vt

synTable :: TCState -> SynTable
synTable (TC _ _ _ _ st _ _) = st

fixTable :: TCState -> FixTable
fixTable (TC _ _ ft _ _ _ _) = ft

uvarSubst :: TCState -> IM.IntMap EType
uvarSubst (TC _ _ _ _ _ _ sub) = sub

moduleName :: TCState -> IdentModule
moduleName (TC mn _ _ _ _ _ _) = mn

putValueTable :: ValueTable -> T ()
putValueTable venv = T.do
  TC mn n fx tenv senv _ m <- get
  put (TC mn n fx tenv senv venv m)

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = T.do
  TC mn n fx _ senv venv m <- get
  put (TC mn n fx tenv senv venv m)

putSynTable :: SynTable -> T ()
putSynTable senv = T.do
  TC mn n fx tenv _ venv m <- get
  put (TC mn n fx tenv senv venv m)

-- Use the type table as the value table, and the primKind table as the type table.
withTypeTable :: forall a . T a -> T a
withTypeTable ta = T.do
  TC mn n fx tt st vt m <- get
  put (TC mn n fx primKindTable M.empty tt m)
  a <- ta
  TC mnr nr _ _ _ ttr mr <- get
  put (TC mnr nr fx ttr st vt mr)
  T.return a

initTC :: IdentModule -> FixTable -> TypeTable -> SynTable -> ValueTable -> TCState
initTC mn fs ts ss vs =
--  trace ("initTC " ++ show (ts, vs)) $
  let
    xts = foldr (uncurry M.insert) ts primTypes
    xvs = foldr (uncurry M.insert) vs primValues
  in TC mn 1 fs xts ss xvs IM.empty

kTypeS :: ETypeScheme
kTypeS = kType

kTypeTypeS :: ETypeScheme
kTypeTypeS = kArrow kType kType

kTypeTypeTypeS :: ETypeScheme
kTypeTypeTypeS = kArrow kType $ kArrow kType kType

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

getArrow :: EType -> Maybe (EType, EType)
getArrow (EApp (EApp (EVar n) a) b) =
  if eqIdent n (mkIdent "->") || eqIdent n (mkIdent "Primitives.->") then Just (a, b) else Nothing
getArrow _ = Nothing

addUVar :: Int -> EType -> T ()
addUVar i t = T.do
  let
    add = T.do
      TC mn n fx tenv senv venv sub <- get
      put (TC mn n fx tenv senv venv (IM.insert i t sub))
  case t of
    EUVar j -> if i == j then T.return () else add
    _ -> add

munify :: --XHasCallStack =>
          SLoc -> Maybe EType -> EType -> T ()
munify _ Nothing _ = T.return ()
munify loc (Just a) b = unify loc a b

expandType :: --XHasCallStack =>
              EType -> T EType
expandType at = T.do
  tt <- derefUVar at
  expandSyn tt

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
              if length vks /= length ts then tcError (getSLocIdent i) $ ": bad synonym use: " --X ++ show (i, vks, ts)
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
      sub <- gets uvarSubst
      case IM.lookup i sub of
        Nothing -> T.return at
        Just t -> derefUVar t
    EVar _ -> T.return at
    ESign t k -> flip ESign k <$> derefUVar t
    EForall iks t -> EForall iks <$> derefUVar t
    _ -> impossible

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
          --trace ("impossible unify " ++ showExpr a ++ " = " ++ showExpr b) $
          impossible
    EUVar i -> addUVar i b
    _ -> impossible

unMType :: Maybe EType -> T EType
unMType mt =
  case mt of
    Nothing -> newUVar
    Just t -> T.return t

-- Reset type variable and unification map
tcReset :: T ()
tcReset = T.do
  TC mn _ fx tenv senv venv _ <- get
  put (TC mn 0 fx tenv senv venv IM.empty)

newUVar :: T EType
newUVar = T.do
  TC mn n fx tenv senv venv sub <- get
  put (TC mn (n+1) fx tenv senv venv sub)
  T.return (EUVar n)

tLookupInst :: --XHasCallStack =>
               String -> Ident -> T (Expr, EType)
tLookupInst msg i = T.do
  (e, s) <- tLookup msg i
--  traceM ("lookup " ++ show (i, s))
  t <- tInst s
  T.return (e, t)

tLookup :: --XHasCallStack =>
           String -> Ident -> T (Expr, ETypeScheme)
tLookup msg i = T.do
  env <- gets valueTable
  case M.lookup i env of
    Nothing -> tcError (getSLocIdent i) $ ": undefined " ++ msg ++ ": " ++ showIdent i
               -- ++ "\n" ++ show env ;
    Just [Entry e s] -> T.return (setSLocExpr (getSLocIdent i) e, s)
    Just _ -> tcError (getSLocIdent i) $ ": ambiguous " ++ showIdent i

tInst :: ETypeScheme -> T EType
tInst as =
  case as of
    EForall vks t ->
      if null vks then T.return t
      else T.do
        let vs = map idKindIdent vks
        us <- T.mapM (const newUVar) (replicate (length vs) ())
        T.return (subst (zip vs us) t)
    t -> T.return t

extValE :: --XHasCallStack =>
           Ident -> ETypeScheme -> Expr -> T ()
extValE i t e = T.do
  venv <- gets valueTable
  putValueTable (M.insert i [Entry e t] venv)

extQVal :: --XHasCallStack =>
           Ident -> ETypeScheme -> T ()
extQVal i t = T.do
  mn <- gets moduleName
  extValE i t (EVar $ qualIdent mn i)

extVal :: --XHasCallStack =>
          Ident -> ETypeScheme -> T ()
extVal i t = extValE i t $ EVar i

extVals :: --XHasCallStack =>
           [(Ident, ETypeScheme)] -> T ()
extVals = T.mapM_ (uncurry extVal)

extTyp :: Ident -> ETypeScheme -> T ()
extTyp i t = T.do
  tenv <- gets typeTable
  putTypeTable (M.insert i [Entry (EVar i) t] tenv)

extTyps :: [(Ident, ETypeScheme)] -> T ()
extTyps = T.mapM_ (uncurry extTyp)

extSyn :: Ident -> ETypeScheme -> T ()
extSyn i t = T.do
  senv <- gets synTable
  putSynTable (M.insert i t senv)

extFix :: Ident -> Fixity -> T ()
extFix i fx = T.do
  TC mn n fenv tenv senv venv sub <- get
  put $ TC mn n (M.insert i fx fenv) tenv senv venv sub
  T.return ()

withExtVal :: forall a . --XHasCallStack =>
              Ident -> ETypeScheme -> T a -> T a
withExtVal i t ta = T.do
  venv <- gets valueTable
  extVal i t
  a <- ta
  putValueTable venv
  T.return a

withExtVals :: forall a . --XHasCallStack =>
               [(Ident, ETypeScheme)] -> T a -> T a
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
    Newtype (i, vks) c t -> withVks vks kType $ \ vvks _  -> T.return $ Newtype (i, vvks) c t
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
          (kkr, _) <- tcTypeT Nothing kr
          T.return (reverse r, kkr)
        loop r (IdKind i k : iks) = T.do
          (kk, _) <- tcTypeT Nothing k
          withExtVal i kk $ loop (IdKind i kk : r) iks
      loop [] vks
  fun nvks nkr

addTypeKind :: EDef -> T ()
addTypeKind adef = T.do
  tcReset
  case adef of
    Data    lhs _   -> addLHSKind lhs kType
    Newtype lhs _ _ -> addLHSKind lhs kType
    Type    lhs t   -> addLHSKind lhs (getTypeKind t)
    _               -> T.return ()

getTypeKind :: EType -> EKind
getTypeKind (ESign _ k) = k
getTypeKind _ = kType

addLHSKind :: LHS -> EKind -> T ()
addLHSKind (i, vks) kret =
--  trace ("addLHSKind " ++ showIdent i ++ " :: " ++ showExpr (lhsKind vks kret)) $
  extQVal i (lhsKind vks kret)

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
    Data    lhs cs   -> Data    lhs   <$> withVars (snd lhs) (T.mapM tcConstr cs)
    Newtype lhs c  t -> Newtype lhs c <$> withVars (snd lhs) (fst <$> tcTypeT (Just kType) t)
    Type    lhs    t -> Type    lhs   <$> withVars (snd lhs) (fst <$> tcTypeT Nothing t)
    Sign    i      t -> (Sign    i   . fst) <$> tcTypeT (Just kType) t
    ForImp  ie i   t -> (ForImp ie i . fst) <$> tcTypeT (Just kType) t
    _ -> T.return d

withVars :: forall a . [IdKind] -> T a -> T a
withVars aiks ta =
  case aiks of
    [] -> ta
    IdKind i k : iks -> T.do
      withExtVal i k $ withVars iks ta

tcConstr :: Constr -> T Constr
tcConstr (i, ts) = (i,) <$> T.mapM (\ t -> fst <$> tcTypeT (Just kType) t) ts

tcDefsValue :: [EDef] -> T [EDef]
tcDefsValue ds = T.do
  T.mapM_ addValueType ds
  T.mapM (\ d -> T.do { tcReset; tcDefValue d}) ds

addValueType :: EDef -> T ()
addValueType adef = T.do
  mn <- gets moduleName
  case adef of
    Sign i t -> T.do
      extQVal i t
      extVal (qualIdent mn i) t
    Data (i, vks) cs -> T.do
      let
        cti = [ (qualIdent mn c, length ts) | (c, ts) <- cs ]
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
        addCon (c, ts) =
          extValE c (EForall vks $ foldr tArrow tret ts) (ECon $ ConData cti (qualIdent mn c))
      T.mapM_ addCon cs
    Newtype (i, vks) c t -> T.do
      let
        tret = foldl tApp (tCon (qualIdent mn i)) (map tVarK vks)
      extValE c (EForall vks $ tArrow t tret) (ECon $ ConNew (qualIdent mn c))
    ForImp _ i t -> T.do
      extQVal i t
      extVal (qualIdent mn i) t
    _ -> T.return ()

unForall :: EType -> ([IdKind], EType)
unForall (EForall iks t) = (iks, t)
unForall t = ([], t)

tcDefValue :: --XHasCallStack =>
              EDef -> T EDef
tcDefValue adef =
  case adef of
    Fcn i eqns -> T.do
--      traceM $ "tcDefValue: " ++ showLHS (i, vs) ++ " = " ++ showExpr rhs
      (_, tt) <- tLookup "no type signature" i
      let (iks, tfn) = unForall tt
      mn <- gets moduleName
      teqns <- withExtTyps iks $ tcEqns tfn eqns
      T.return $ Fcn (qualIdent mn i) teqns
    ForImp ie i t -> T.do
      mn <- gets moduleName
      T.return (ForImp ie (qualIdent mn i) t)
    _ -> T.return adef

-- Kind check a type while already in type checking mode
tcTypeT :: --XHasCallStack =>
           Maybe EKind -> EType -> T (Typed EType)
tcTypeT mk = tcExpr mk . dsType

-- Kind check a type while in value checking mode
tcType :: --XHasCallStack =>
          Maybe EKind -> EType -> T (Typed EType)
tcType mk = withTypeTable . tcTypeT mk

{-
-- Sort check a kind while already in type cheking mode
tcKind :: --XHasCallStack =>
          EKind -> T EKind
tcKind e = fst <$> withTypeTable (tcType (Just kType) e)
-}

tcExpr :: --XHasCallStack =>
          Maybe EType -> Expr -> T (Typed Expr)
tcExpr mt ae = T.do
--  traceM ("tcExpr enter: " ++ showExpr ae ++ " :: " ++ showMaybe showExpr mt)
  r <- tcExprR mt ae
--  t <- expandType (snd r)
--  traceM ("tcExpr exit: " ++ showExpr (fst r) ++ " :: " ++ showExpr t)
  T.return r
tcExprR :: --XHasCallStack =>
           Maybe EType -> Expr -> T (Typed Expr)
tcExprR mt ae =
  let { loc = getSLocExpr ae } in
  case ae of
    EVar i ->
      if isUnderscore i then
        -- this only happens with patterns translated into expressions
        (ae,) <$> newUVar
      else T.do
        (e, t) <- tLookupInst "variable" i
        case mt of
          Just tu@(EForall _ tt) -> T.do
            -- XXX This is wrong in many ways.
            -- Both t and tt may contain unification variables bound elsewhere.
            unify loc tt t
            T.return (e, tu)
          _ -> T.do
            munify loc mt t
            T.return (e, t)
    EApp f a -> T.do
      (ef, tf) <- tcExpr Nothing f
      (ta, tr) <- unArrow loc tf
      (ea, _) <- tcExpr (Just ta) a
      munify loc mt tr
      T.return (EApp ef ea, tr)
{- slower and uses more memory
      (ea, ta) <- tcExpr Nothing a
      tr <- unMType mt
      (ef, _) <- tcExpr (Just (tArrow ta tr)) f
      T.return (EApp ef ea, tr)
-}
    EOper e ies -> tcOper mt e ies
    ELam is e -> tcExprLam mt is e
    ELit loc' l -> tcLit mt loc' l
    ECase a arms -> T.do
      (ea, ta) <- tcExpr Nothing a
      tt <- unMType mt
      earms <- T.mapM (tcArm tt ta) arms
      T.return (ECase ea earms, tt)
    ELet bs a -> tcBinds bs $ \ ebs -> T.do { (ea, ta) <- tcExpr mt a; T.return (ELet ebs ea, ta) }
    ETuple es -> T.do
      let
        n = length es
      (ees, tes) <- T.fmap unzip (T.mapM (tcExpr Nothing) es)
      let
        ttup = tApps (tupleConstr loc n) tes
      munify loc mt ttup
      T.return (ETuple ees, ttup)
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
    ESectR i e ->
      tcExpr mt (ELam [eVarI loc "$x"] (EApp (EApp (EVar i) (eVarI loc"$x")) e))
    EIf e1 e2 e3 -> T.do
      (ee1, _) <- tcExpr (Just (tBool (getSLocExpr e1))) e1
      (ee2, te2) <- tcExpr mt e2
      (ee3, te3) <- tcExpr mt e3
      unify loc te2 te3
      T.return (EIf ee1 ee2 ee3, te2)
    EListish (LList es) -> T.do
      (ees, ts) <- T.fmap unzip (T.mapM (tcExpr Nothing) es)
      te <- case ts of
              [] -> newUVar
              t : _ -> T.return t
      let
        tlist = tApp (tList loc) te
      munify loc mt tlist
      T.return (EListish (LList ees), tlist)
    EListish (LCompr eret ass) -> T.do
      let
        doStmts :: [EStmt] -> [EStmt] -> T ([EStmt], Typed Expr)
        doStmts rss xs =
          case xs of
            [] -> T.do
              r <- tcExpr Nothing eret
              T.return (reverse rss, r)
            as : ss ->
              case as of
                SBind p a -> T.do
                  v <- newUVar
                  (ea, _) <- tcExpr (Just $ tApp (tList loc) v) a
                  tcPat v p $ \ ep ->
                    doStmts (SBind ep ea : rss) ss
                SThen a -> T.do
                  (ea, _) <- tcExpr (Just (tBool (getSLocExpr a))) a
                  doStmts (SThen ea : rss) ss
                SLet bs ->
                  tcBinds bs $ \ ebs ->
                    doStmts (SLet ebs : rss) ss
      (rss, (ea, ta)) <- doStmts [] ass
      let
        tr = tApp (tList loc) ta
      munify loc mt tr
      T.return (EListish (LCompr ea rss), tr)
    EListish (LFrom       e)        -> tcExpr mt (enum loc "From" [e])
    EListish (LFromTo     e1 e2)    -> tcExpr mt (enum loc "FromTo" [e1, e2])
    EListish (LFromThen   e1 e2)    -> tcExpr mt (enum loc "FromThen" [e1,e2])
    EListish (LFromThenTo e1 e2 e3) -> tcExpr mt (enum loc "FromThenTo" [e1,e2,e3])
    ESign e t -> T.do
      (tt, _) <- tcType (Just kType) t
      (ee, _) <- tcExpr (Just tt) e
      munify loc mt tt
      T.return (ee, tt)
    EAt i e -> T.do
      (ee, t) <- tcExpr mt e
      (_, ti) <- tLookupInst "impossible!" i
      unify loc t ti
      T.return (EAt i ee, t)
    EForall vks t ->
      withVks vks kType $ \ vvks _ -> T.do
        (tt, k) <- withVars vvks (tcExpr mt t)
        T.return (EForall vvks tt, k)
    _ -> impossible

enum :: SLoc -> String -> [Expr] -> Expr
enum loc f = foldl EApp (EVar (mkIdentSLoc loc ("enum" ++ f)))

tcLit :: Maybe EType -> SLoc -> Lit -> T (Typed Expr)
tcLit mt loc l =
  let { lit t = T.do { munify loc mt t; T.return (ELit loc l, t) } } in
  case l of
    LInt _  -> lit (tConI loc "Primitives.Int")
    LDouble _ -> lit (tConI loc "Primitives.Double")
    LChar _ -> lit (tConI loc "Primitives.Char")
    LStr _  -> lit (tApp (tConI loc "Data.List.[]") (tConI loc "Primitives.Char"))
    LPrim _ -> T.do
      t <- unMType mt  -- pretend it is anything
      T.return (ELit loc l, t)
    LForImp _ -> impossible


tcOper :: Maybe EType -> Expr -> [(Ident, Expr)] -> T (Typed Expr)
tcOper mt ae aies = T.do
  let
    appOp (f, ft) (e1, t1) (e2, t2) = T.do
      let l = getSLocExpr f
      (fta1, ftr1) <- unArrow l ft
      (fta2, ftr2) <- unArrow l ftr1
      unify l fta1 t1
      unify l fta2 t2
--      traceM (showExpr (EApp (EApp f e1) e2))
      T.return (EApp (EApp f e1) e2, ftr2)

    doOp (e1:e2:es) o os ies = T.do
      e <- appOp o e2 e1
      calc (e:es) os ies
    doOp _ _ _ _ = impossible

    calc :: [Typed Expr] -> [(Typed Expr, Fixity)] -> [((Typed Expr, Fixity), Expr)] -> T (Typed Expr) 
    calc [et@(_, t)] [] [] = T.do munify (getSLocExpr ae) mt t; T.return et
    calc es ((o, _):os) [] = doOp es o os []
    calc es oos@((oy, (ay, py)):os) iies@((oo@(ox, (ax, px)), e) : ies) = T.do
--      traceM (show ((unIdent (getIdent (fst o)), ay, py), (unIdent i, ax, px)))
      if px == py && (not (eqAssoc ax ay) || eqAssoc ax AssocNone) then
        tcError (getSLocExpr (fst ox)) "Ambiguous operator expression"
       else if px < py || eqAssoc ax AssocLeft && px == py then
        doOp es oy os iies
       else T.do
        et <- tcExpr Nothing e
        calc (et:es) (oo : oos) ies
    calc es [] ((o, e) : ies) = T.do
      ee <- tcExpr Nothing e
      calc (ee:es) [o] ies
    calc _ _ _ = impossible

    opfix fixs (i, e) = T.do
      o@(ei, _) <- tcExpr Nothing (EVar i)
      let fx = getFixity fixs (getIdent ei)
      T.return ((o, fx), e)

  aet <- tcExpr Nothing ae
  fixs <- gets fixTable
--  traceM $ unlines $ map show [(unIdent i, fx) | (i, fx) <- M.toList fixs]
  ites <- T.mapM (opfix fixs) aies
  et@(_, t) <- calc [aet] [] ites
  munify (getSLocExpr ae) mt t
  T.return et

unArrow :: SLoc -> EType -> T (EType, EType)
unArrow loc t =
  case getArrow t of
    Just ar -> T.return ar
    Nothing -> T.do
      a <- newUVar
      r <- newUVar
      unify loc t (tArrow a r)
      T.return (a, r)

getFixity :: FixTable -> Ident -> Fixity
getFixity fixs i = fromMaybe (AssocLeft, 9) $ M.lookup i fixs

tcPats :: forall a . EType -> [EPat] -> (EType -> [Typed EPat] -> T a) -> T a
tcPats t [] ta = ta t []
tcPats t (p:ps) ta = T.do
  (tp, tr) <- unArrow (getSLocExpr p) t
  tcPat tp p $ \ pp -> tcPats tr ps $ \ tt pps -> ta tt ((pp, tp) : pps)

tcExprLam :: Maybe EType -> [EPat] -> Expr -> T (Typed Expr)
tcExprLam mt aps expr = T.do
  t <- unMType mt
  tcPats t aps $ \ tt pts -> T.do
    (er, tr) <- tcExpr (Just tt) expr
    T.return (ELam (map fst pts) er, foldr tArrow tr (map snd pts))

tcEqns :: EType -> [Eqn] -> T [Eqn]
tcEqns t eqns = T.mapM (tcEqn t) eqns

tcEqn :: EType -> Eqn -> T Eqn
tcEqn t eqn =
  case eqn of
    Eqn ps alts -> tcPats t ps $ \ tt tps -> T.do
      aalts <- tcAlts tt alts
      T.return (Eqn (map fst tps) aalts)

tcAlts :: EType -> EAlts -> T EAlts
tcAlts tt (EAlts alts bs) =
  tcBinds bs $ \ bbs -> T.do { aalts <- T.mapM (tcAlt tt) alts; T.return (EAlts aalts bbs) }

tcAlt :: EType -> EAlt -> T EAlt
tcAlt t (ss, rhs) = tcGuards ss $ \ sss -> T.do { (rrhs,_) <- tcExpr (Just t) rhs; T.return (sss, rrhs) }

tcGuards :: forall a . [EStmt] -> ([EStmt] -> T a) -> T a
tcGuards [] ta = ta []
tcGuards (s:ss) ta = tcGuard s $ \ rs -> tcGuards ss $ \ rss -> ta (rs:rss)

tcGuard :: forall a . EStmt -> (EStmt -> T a) -> T a
tcGuard (SBind p e) ta = T.do
  (ee, tt) <- tcExpr Nothing e
  tcPat tt p $ \ pp -> ta (SBind pp ee)
tcGuard (SThen e) ta = T.do
  (ee, _) <- tcExpr (Just (tBool (getSLocExpr e))) e
  ta (SThen ee)
tcGuard (SLet bs) ta = tcBinds bs $ \ bbs -> ta (SLet bbs)

tcArm :: EType -> EType -> ECaseArm -> T ECaseArm
tcArm t tpat arm =
  case arm of
    (p, alts) -> tcPat tpat p $ \ pp -> T.do
      aalts <- tcAlts t alts
      T.return (pp, aalts)

tcPat ::forall a .  EType -> EPat -> (EPat -> T a) -> T a
tcPat t p@(EVar v) ta | not (isConIdent v) = T.do  -- simple special case
  withExtVals [(v, t)] $ ta p
tcPat t ap ta = T.do
--  traceM $ "tcPat: " ++ show ap
  env <- T.mapM (\ v -> (v,) <$> newUVar) $ filter (not . isUnderscore) $ patVars ap
  withExtVals env $ T.do
    (pp, _) <- tcExpr (Just t) ap
    () <- checkArity (getSLocExpr ap) 0 pp
    ta pp

checkArity :: SLoc -> Int -> EPat -> T ()
checkArity loc n (EApp f _) = checkArity loc (n+1) f
checkArity loc n (ECon c) = if n == conArity c then T.return () else tcError loc ": con arity"
checkArity _ _ _ = T.return ()

-- XXX No mutual recursion yet
tcBinds :: forall a . [EBind] -> ([EBind] -> T a) -> T a
tcBinds xbs ta = T.do
  let
    tmap = M.fromList [ (i, t) | BSign i t <- xbs ]
    xs = concatMap getBindVars xbs
  xts <- T.mapM (tcBindVarT tmap) xs
  withExtVals xts $ T.do
    nbs <- T.mapM tcBind xbs
    ta nbs

tcBindVarT :: M.Map ETypeScheme -> Ident -> T (Ident, ETypeScheme)
tcBindVarT tmap x = T.do
  case M.lookup x tmap of
    Nothing -> T.do
      t <- newUVar
      T.return (x, t)
    Just t -> T.do
      tt <- fst <$> (withTypeTable $ tcTypeT (Just kType) t)
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
      (ep, tp) <- tcExpr Nothing p
      (ea, _)  <- tcExpr (Just tp) a
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

tList :: SLoc -> EType
tList loc = tConI loc "Data.List.[]"

tBool :: SLoc -> EType
tBool loc = tConI loc "Data.Bool_Type.Bool"

impossible :: --XHasCallStack =>
              forall a . a
impossible = error "impossible"

showTModule :: forall a . (a -> String) -> TModule a -> String
showTModule sh amdl =
  case amdl of
    TModule mn _ _ _ _ a -> "Tmodule " ++ showIdent mn ++ "\n" ++ sh a

isUnderscore :: Ident -> Bool
isUnderscore = eqString "_" . unIdent

{-
showValueTable :: ValueTable -> String
showValueTable vt =
  unlines $ take 5 [showIdent i ++ " : " ++ showExpr t | (i, [Entry _ t]) <- M.toList vt]
-}
