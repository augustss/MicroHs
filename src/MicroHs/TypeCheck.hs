{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE QualifiedDo #-}
module MicroHs.TypeCheck(
  --module MicroHs.TypeCheck
  typeCheck,
  TModule(..),
  showTModule,
  ) where
import Prelude
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import Control.Monad.State.Strict as T --Xhiding(ap)
import qualified MicroHs.StringMap as M
import MicroHs.Parse
--Ximport Compat
import Debug.Trace
import GHC.Stack

data TModule a = TModule IdentModule [TypeExport] [ValueExport] a
  --Xderiving (Show)

data TypeExport = TypeExport Ident Ident TypeInfo  -- exported name, original name
  --Xderiving (Show)

data ValueExport = ValueExport Ident Ident ETypeScheme
  --Xderiving (Show)

typeCheck :: [(ImportSpec, TModule a)] -> EModule -> TModule [EDef]
typeCheck imps amdl =
--  trace (show amdl) $
  let (ts, vs) = mkTables imps
  in  case amdl of
        EModule mn exps defs ->
          case runState (tcDefs defs) (initTC mn ts vs) of
            (tds, _) ->
              let
                thisMdl = (mn, mkTModule mn tds impossible)
                impMdls = [(fromMaybe m mm, tm) | (ImportSpec _ m mm, tm) <- imps]
                impMap = M.fromList (thisMdl : impMdls)
                (texps, vexps) = unzip [ (te, ve) | ExpModule m <- exps,
                                         let TModule _ te ve _ = fromMaybe (error $ "import " ++ m) (M.lookup m impMap) ]
              in  TModule mn (concat texps) (concat vexps) tds

mkTModule :: IdentModule -> [EDef] -> a -> TModule a
mkTModule mn tds a =
  let
    ves = [ ValueExport i (qual mn i) t | Sign i t <- tds ]
    con it vs (ic, ts) = (ic, ETypeScheme vs (foldr tArrow (tApps (qual mn it) (map tVar vs)) ts))
    tes = [ TypeExport  i (qual mn i) (TConc (lhsKind vs) (map (con i vs) cs)) | Data (i, vs) cs <- tds ] ++
          [ TypeExport  i (qual mn i) (TSyn  (lhsKind vs) (ETypeScheme vs t))  | Type (i, vs) t  <- tds ]
  in  TModule mn tes ves a

mkTables :: [(ImportSpec, TModule a)] -> (TypeTable, ValueTable)
mkTables mdls =
  let
    qns aisp mn i =
      case aisp of
        ImportSpec q _ mas ->
          let
            m = fromMaybe mn mas
          in  if q then [qual m i] else [i, qual m i]
    --XallValues :: M.Map [Entry]
    allValues =
      let
        con mn ti i = ECon [(qual mn c, arityOf t) | (c, ETypeScheme _ t) <- constrs ti] (qual mn i)
        syms arg =
          case arg of
            (is, TModule mn tes ves _) ->
              [ (v, [Entry (EVar qi) t])     | ValueExport i qi t <- ves,                       v <- qns is mn i ] ++
              [ (v, [Entry (con mn ti i) t]) | TypeExport  _ _ ti <- tes, (i, t) <- constrs ti, v <- qns is mn i ]
      in  M.fromListWith (unionBy eqEntry) $ concatMap syms mdls
    allSyns =
      let
        syns arg =
          case arg of
            (_, TModule _ tes _ _) -> [(qi, ts) | TypeExport _ qi (TSyn _ ts) <- tes ]
      in  M.fromList (concatMap syns mdls)
    --XallTypes :: TypeTable
    allTypes =
      let
        types arg =
          case arg of
            (is, TModule mn tes _ _) -> [ (v, [Entry (EVar qi) (kindOf ti)]) | TypeExport i qi ti <- tes, v <- qns is mn i ]
      in M.fromListWith (unionBy eqEntry) $ concatMap types mdls
  in  (allTypes, (allValues, allSyns))

arityOf :: EType -> Int
arityOf at =
  case getArrow at of
    Nothing -> 0
    Just (_, r) -> 1 + arityOf r

constrs :: TypeInfo -> [(Ident, ETypeScheme)]
constrs ti =
  case ti of
    TAbs _ -> []
    TConc _ cs -> cs
    TSyn _ _ -> []

kindOf :: TypeInfo -> ETypeScheme
kindOf ti =
  case ti of
    TAbs k -> ETypeScheme [] k
    TConc k _ -> ETypeScheme [] k
    TSyn k _ -> ETypeScheme [] k

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
    ECon _ i -> i
    _ -> impossible

--------------------------

type Typed a = (a, EType)

data Entry = Entry Expr ETypeScheme
  --Xderiving(Show)

type ValueTable = (M.Map [Entry], M.Map ETypeScheme)
type TypeTable  = M.Map [Entry]

data TCState = TC IdentModule Int TypeTable ValueTable (IM.IntMap EType)
  --Xderiving (Show)

typeTable :: TCState -> TypeTable
typeTable ts =
  case ts of
    TC _ _ tt _ _ -> tt

valueTable :: TCState -> ValueTable
valueTable ts =
  case ts of
    TC _ _ _ vt _ -> vt

uvarSubst :: TCState -> IM.IntMap EType
uvarSubst ts =
  case ts of
    TC _ _ _ _ sub -> sub

moduleName :: TCState -> IdentModule
moduleName ts =
  case ts of
    TC mn _ _ _ _ -> mn

putValueTable :: ValueTable -> T ()
putValueTable venv = T.do
  TC mn n tenv _ m <- get
  put (TC mn n tenv venv m)

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = T.do
  TC mn n _ venv m <- get
  put (TC mn n tenv venv m)

-- Use the type table as the value table, and an empty type table
withTypeTable :: T a -> T a
withTypeTable ta = T.do
  TC mn n tt vt m <- get
  put (TC mn n M.empty (tt, M.empty) m)
  a <- ta
  TC mn' n' _ (tt', _) m' <- get
  put (TC mn' n' tt' vt m')
  T.return a

initTC :: IdentModule -> TypeTable -> ValueTable -> TCState
initTC mn ts vs =
--  trace ("initTC " ++ show (ts, vs)) $
  let
    xts = foldr (uncurry M.insert) ts primTypes
  in TC mn 1 xts vs IM.empty

primTypes :: [(Ident, [Entry])]
primTypes =
  let
    entry i = Entry (EVar i)
    tuple n =
      let i = tupleConstr n
      in  (i, [entry i $ ETypeScheme [] $ foldr kArrow kType (replicate n kType)])
    t = ETypeScheme [] kType
    tt = ETypeScheme [] $ kArrow kType kType
    ttt = ETypeScheme [] $ kArrow kType $ kArrow kType kType
  in  
      [("IO",     [entry "Primitives.IO"       tt]),
       ("->",     [entry "Primitives.->"       ttt]),
       ("Int",    [entry "Primitives.Int"      t]),
       ("Char",   [entry "Primitives.Char"     t]),
       ("Handle", [entry "Primitives.Handle"   t]),
       ("String", [entry "Data.Char.String"    t]),
       ("[]",     [entry "Data.List.[]"        tt]),
       ("()",     [entry "Data.Tuples.()"      t]),
       ("Bool",   [entry "Data.Bool_Type.Bool" t])] ++
      map tuple (enumFromTo 2 10)

type T a = State TCState a

tCon :: Ident -> EType
tCon = EVar

tVar :: Ident -> EType
tVar = EVar

tApp :: EType -> EType -> EType
tApp = EApp

tApps :: Ident -> [EType] -> EType
tApps i ts = foldl tApp (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = tApp (tApp (tCon "Primitives.->") a) r

kArrow :: EKind -> EKind -> EKind
kArrow = tArrow

kType :: EKind
kType = EVar "Type"

getArrow :: EType -> Maybe (EType, EType)
getArrow at =
  case at of
    EApp f a ->
      case f of
        EApp arr b ->
          case arr of
            EVar n -> if eqIdent n "->" || eqIdent n "Primitives.->" then Just (b, a) else Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

addUVar :: Int -> EType -> T ()
addUVar i t = T.do
  let
    add = T.do
      TC mn n tenv venv sub <- get
      put (TC mn n tenv venv (IM.insert i t sub))
  case t of
    EUVar j -> if i == j then T.return () else add
    _ -> add

munify :: Maybe EType -> EType -> T ()
munify mt b =
  case mt of
    Nothing -> T.return ()
    Just a -> unify a b

expandType :: EType -> T EType
expandType at = T.do
  tt <- derefUVar at
  expandSyn tt

expandSyn :: EType -> T EType
expandSyn at =
  let
    syn ts t =
      case t of
        EApp f a -> syn (f:ts) a
        EVar i -> T.do
          synTable <- gets (snd . valueTable)
          case M.lookup i synTable of
            Nothing -> T.return at
            Just (ETypeScheme vs tt) ->
              if length vs /= length ts then error "bad syn app" else
              expandSyn $ subst (zip vs ts) tt
        _ -> T.return at
  in syn [] at

derefUVar :: EType -> T EType
derefUVar at =
  case at of
    EApp f a -> EApp <$> expandType f <*> expandType a
    EUVar i -> T.do
      sub <- gets uvarSubst
      case IM.lookup i sub of
        Nothing -> T.return at
        Just t -> derefUVar t
    _ -> T.return at

unify :: EType -> EType -> T ()
unify a b = T.do
  aa <- expandType a
  bb <- expandType b
  unifyR aa bb
  
unifyR :: EType -> EType -> T ()
unifyR a b = T.do
  let bad = error $ "Cannot unify " ++ showExpr a ++ " and " ++ showExpr b
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
        EApp fb xb -> T.do { unify fa fb; unify xa xb }
        EUVar i    -> addUVar i a
        _          -> impossible
    EUVar i -> addUVar i b
    _ -> impossible

unMType :: Maybe EType -> T EType
unMType mt =
  case mt of
    Nothing -> newUVar
    Just t -> T.return t

newUVar :: T EType
newUVar = T.do
  TC mn n tenv venv sub <- get
  put (TC mn (n+1) tenv venv sub)
  T.return (EUVar n)

tLookupInst :: Ident -> T (Expr, EType)
tLookupInst i = T.do
  (e, s) <- tLookup i
  t <- tInst s
  return (e, t)

tLookup :: Ident -> T (Expr, ETypeScheme)
tLookup i = T.do
  env <- gets (fst . valueTable)
  case M.lookup i env of
    Nothing -> error $ "undefined variable " ++ i ++ "\n" ++ show env
    Just aes ->
      case aes of
        [] -> impossible
        Entry e s : es ->
          if null es then
            T.return (e, s)
          else
            error "ambiguous"

tInst :: ETypeScheme -> T EType
tInst as =
  case as of
    ETypeScheme vs t ->
      if null vs then T.return t
      else T.do
        us <- mapM (const newUVar) (replicate (length vs) ())
        T.return (subst (zip vs us) t)

extQValE :: Ident -> ETypeScheme -> Expr -> T ()
extQValE i t e = T.do
  venv <- gets valueTable
  putValueTable (M.insert i [Entry e t] (fst venv), snd venv)

extQVal :: Ident -> ETypeScheme -> T ()
extQVal i t = T.do
  mn <- gets moduleName
  extQValE i t (EVar $ qual mn i)

extVal :: Ident -> ETypeScheme -> T ()
extVal i t = T.do
  venv <- gets valueTable
  putValueTable (M.insert i [Entry (EVar i) t] (fst venv), snd venv)

extTyp :: Ident -> ETypeScheme -> T ()
extTyp i t = T.do
  tenv <- gets typeTable
  putTypeTable (M.insert i [Entry (EVar i) t] tenv)

extVals :: [(Ident, ETypeScheme)] -> T ()
extVals env = T.do
  venv <- gets valueTable
  let
    ins it =
      case it of
        (i, t) -> M.insert i [Entry (EVar i) t]
  putValueTable (foldr ins (fst venv) env, snd venv)

extSyns :: Ident -> ETypeScheme -> T ()
extSyns i t = T.do
  venv <- gets valueTable
  putValueTable (fst venv, M.insert i t (snd venv))

withExtVal :: Ident -> ETypeScheme -> T a -> T a
withExtVal i t ta = T.do
  venv <- gets valueTable
  extVal i t
  a <- ta
  putValueTable venv
  T.return a

withExtVals :: [(Ident, ETypeScheme)] -> T a -> T a
withExtVals env ta = T.do
  venv <- gets valueTable
  extVals env
  a <- ta
  putValueTable venv
  T.return a

--lookupPrimType :: String -> EType
--lookupPrimType = undefined

{-
unMTypeArrow :: Maybe EType -> T (EType, Maybe EType)
unMTypeArrow mt =
  case mt of
    Nothing -> T.do
      t <- newUVar
      T.return (t, Nothing)
    Just tarr -> T.do
      case getArrow tarr of
        Just (ta, tr) -> (ta, Just tr)
	Nothing -> T.do
-}

tcDefs :: [EDef] -> T [EDef]
tcDefs ds = T.do
--  traceM ("tcDefs ds=" ++ show ds)
  dst <- tcDefsType ds
  mapM_ addTypeSyn ds
--  traceM ("tcDefs dst=" ++ show dst)
  tcDefsValue dst

tcDefsType :: [EDef] -> T [EDef]
tcDefsType ds = withTypeTable $ T.do
  mapM_ addTypeKind ds
  mapM tcDefType ds

addTypeKind :: EDef -> T ()
addTypeKind d =
  case d of
    Data lhs _ -> addLHSKind lhs
    Type lhs _ -> addLHSKind lhs
    _          -> T.return ()

addLHSKind :: LHS -> T ()
addLHSKind (i, vs) = extVal i (ETypeScheme [] $ lhsKind vs)

lhsKind :: [Ident] -> EKind
lhsKind vs = foldr (\ _ -> kArrow kType) kType vs

-- Add type synonyms to the value table
addTypeSyn :: EDef -> T ()
addTypeSyn adef =
  case adef of
    Type (i, vs) t -> extSyns i (ETypeScheme vs t)
    _ -> T.return ()

tcDefType :: EDef -> T EDef
tcDefType d =
  case d of
    Data lhs cs -> Data lhs <$> withVars (lhsKinds lhs) (mapM tcConstr cs)
    Type lhs t  -> Type lhs <$> withVars (lhsKinds lhs) (fst <$> tcType (Just kType) t)
    Sign i t    -> Sign i   <$> tcTypeScheme (Just kType) t
    _ -> T.return d

tcTypeScheme :: Maybe EKind -> ETypeScheme -> T ETypeScheme
tcTypeScheme mk ts =
  case ts of
    ETypeScheme vs t -> ETypeScheme vs <$> withVars (lhsKinds (impossible, vs)) (fst <$> tcType mk t)

lhsKinds :: LHS -> [(Ident, ETypeScheme)]
lhsKinds lhs =
  case lhs of
    (_, vs) -> zip vs (repeat (ETypeScheme [] kType))

withVars :: [(Ident, ETypeScheme)] -> T a -> T a
withVars aiks ta =
  case aiks of
    [] -> ta
    (i,k) : iks -> withExtVal i k $ withVars iks ta

tcConstr :: Constr -> T Constr
tcConstr con =
  case con of
    (i, ts) -> (,) i <$> mapM (\ t -> fst <$> tcType (Just kType) t) ts

tcDefsValue :: [EDef] -> T [EDef]
tcDefsValue ds = T.do
  mapM_ addValueType ds
  mapM tcDefValue ds

addValueType :: EDef -> T ()
addValueType d = T.do
  case d of
    Sign i t -> extQVal i t
    Data (i, vs) cs -> T.do
      mn <- gets moduleName
      let
        cti = [ (qual mn c, length ts) | (c, ts) <- cs ]
        tret = foldl tApp (tCon i) (map tVar vs)
        addCon con =
          case con of
            (c, ts) -> extQValE c (ETypeScheme vs $ foldr tArrow tret ts) (ECon cti (qual mn c))
      mapM_ addCon cs
    _ -> T.return ()

tcDefValue :: EDef -> T EDef
tcDefValue d =
  case d of
    Fcn (i, vs) rhs -> T.do
      (_, ETypeScheme tvs t) <- tLookup i
      mapM_ (uncurry extTyp) (zip tvs (repeat (ETypeScheme [] kType)))
      (et, _tt) <- tcExpr (Just t) $ foldr ELam rhs vs
      mn <- gets moduleName
      T.return $ Fcn (qual mn i, vs) $ dropLam (length vs) et
    _ -> T.return d

dropLam :: Int -> Expr -> Expr
dropLam n ae =
  if n == 0 then
    ae
  else
    case ae of
      ELam _ e -> dropLam (n-1) e
      _        -> impossible

tcType :: Maybe EKind -> EType -> T (Typed EType)
tcType mk = tcExpr mk . dsType

tcExpr :: Maybe EType -> Expr -> T (Typed Expr)
tcExpr mt ae = T.do
--  traceM ("tcExpr enter: " ++ show (ae, mt))
  r <- tcExpr' mt ae
--  traceM ("tcExpr exit: " ++ show r)
  T.return r
tcExpr' :: Maybe EType -> Expr -> T (Typed Expr)
tcExpr' mt ae =
  case ae of
    EVar i ->
      if isUnderscore i then
        -- this only happens with patterns translated into expressions
        (,) ae <$> newUVar
      else T.do
        (e, t) <- tLookupInst i
        munify mt t
        T.return (e, t)
    EApp f a -> T.do
      (ea, ta) <- tcExpr Nothing a
      tr <- unMType mt
      (ef, _) <- tcExpr (Just (tArrow ta tr)) f
      T.return (EApp ef ea, tr)
    ELam i e -> T.do
      ta <- newUVar
      (ee, tr) <- withExtVal i (ETypeScheme [] ta) $ tcExpr Nothing e
      let
        tlam = tArrow ta tr
      munify mt tlam
      T.return (ELam i ee, tlam)
    EInt _ -> T.return (ae, tCon "Primitives.Int")
    EChar _ -> T.return (ae, tCon "Primitives.Char")
    EStr _ -> T.return (ae, tApps "Data.List.[]" [tCon "Primitives.Char"])
    ECase a arms -> T.do
      (ea, ta) <- tcExpr Nothing a
      (earms, tarms) <- unzip <$> mapM (tcArm mt ta) arms
      T.return (ECase ea earms, head tarms)
    ELet bs a -> tcBinds bs $ \ ebs -> T.do { (ea, ta) <- tcExpr mt a; T.return (ELet ebs ea, ta) }
    ETuple es -> T.do
      let
        n = length es
      (ees, tes) <- T.fmap unzip (T.mapM (tcExpr Nothing) es)
      let
        ttup = tApps (tupleConstr n) tes
      munify mt ttup
      T.return (ETuple ees, ttup)
    EList es -> T.do
      (ees, ts) <- T.fmap unzip (T.mapM (tcExpr Nothing) es)
      te <- case ts of
              [] -> newUVar
              t : _ -> T.return t
      let
        tlist = tApps "Data.List.[]" [te]
      munify mt tlist
      T.return (EList ees, tlist)
    EDo _ _ -> undefined
    EPrim _p -> T.do
      t <- newUVar  -- pretend it's anything
      T.return (ae, t)
    ESectL e i -> T.do
      (x, t) <- tcExpr mt (EApp (EVar i) e)
      let EApp (EVar i') e' = x
      T.return (ESectL e' i', t)
    ESectR i e -> T.do
      (x, t) <- tcExpr mt (ELam "$x" (EApp (EApp (EVar i) (EVar "$x")) e))
      let EApp (EApp (EVar i') _) e' = x
      T.return (ESectR i' e', t)
    EIf e1 e2 e3 -> T.do
      (ee1, _) <- tcExpr (Just tBool) e1
      (ee2, te2) <- tcExpr mt e2
      (ee3, te3) <- tcExpr mt e3
      unify te2 te3
      T.return (EIf ee1 ee2 ee3, te2)
    ECompr eret ass -> T.do
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
                  (ea, _) <- tcExpr (Just $ tApp tList v) a
                  tcPat (Just v) p $ \ ep ->
                    doStmts (SBind ep ea : rss) ss
                SThen a -> T.do
                  (ea, _) <- tcExpr (Just tBool) a
                  doStmts (SThen ea : rss) ss
                SLet bs ->
                  tcBinds bs $ \ ebs ->
                    doStmts (SLet ebs : rss) ss
      (rss, (ea, ta)) <- doStmts [] ass
      let
        tr = tApp tList ta
      munify mt tr
      T.return (ECompr ea rss, tr)
    -----
    EBad -> impossible    -- shouldn't happen
    EUVar _ -> impossible -- shouldn't happen
    ECon _ _ -> impossible

tcArm :: Maybe EType -> EType -> ECaseArm -> T (Typed ECaseArm)
tcArm mt t arm =
  case arm of
    (p, a) -> T.do
      (pp, (ea, ta)) <- tcPat (Just t) p $ \ pp -> (,) pp <$> tcExpr mt a
      pure ((pp, ea), ta)

tcPat :: Maybe EType -> EPat -> (EPat -> T a) -> T a
tcPat mt ap ta = T.do
  traceM $ "tcPat: " ++ show ap
  env <- mapM (\ v -> (,) v . ETypeScheme [] <$> newUVar) $ filter (not . isUnderscore) $ patVars ap
  withExtVals env $ T.do
    (ep, _) <- tcExpr mt (ePatToExpr ap)
    pp <- exprToEPat ep
    ta pp

-- XXX No mutual recursion yet
tcBinds :: [EBind] -> ([EBind] -> T a) -> T a
tcBinds xbs ta =
  let
    doBind rbs bbs =
      case bbs of
        [] -> ta (reverse rbs)
        b : bs ->
          case b of
            BFcn (i, vs) a -> undefined
            BPat p a -> undefined
  in doBind [] xbs

--tcStmts :: EType -> EType -> [EStmt] -> T a -> T ([EStmt], Typed a)
--tcStmts tbind tthen ss ta = undefined

-- Desugar [T] and (T,T,...)
dsType :: EType -> EType
dsType at =
  case at of
    EVar _ -> at
    EApp f a -> EApp (dsType f) (dsType a)
    EList [t] -> tApps listConstr [dsType t]
    ETuple ts -> tApps (tupleConstr (length ts)) (map dsType ts)
    _ -> impossible

listConstr :: Ident
listConstr = "[]"

tList :: EType
tList = tCon "Data.List.[]"

tBool :: EType
tBool = tCon "Data.Bool_Type.Bool"

ePatToExpr :: EPat -> Expr
ePatToExpr ap =
  case ap of
    PVar i -> EVar i
    PConstr _ c ps ->
      if eqChar (head c) ',' then
        ETuple (map ePatToExpr ps)
      else
        foldl EApp (EVar c) (map ePatToExpr ps)

exprToEPat :: Expr -> T EPat
exprToEPat =
  let
    to :: [EPat] -> Expr -> T EPat
    to ps ae =
      case ae of
        EVar i -> if null ps then pure $ PVar i else impossible
        ECon cs i -> pure $ PConstr cs i ps
        ETuple es -> T.do
          let
            n = length es
            c = tupleConstr n
            cti = [(c, n)]
          xps <- mapM exprToEPat es
          pure $ PConstr cti c xps
        EApp f a -> T.do
          p <- exprToEPat a
          to (p : ps) f
        _ -> impossible
  in  to []

impossible :: HasCallStack => forall a . a
impossible = error "impossible"

showTModule :: (a -> String) -> TModule a -> String
showTModule sh amdl =
  case amdl of
    TModule mn _texps _vexps a -> "Tmodule " ++ mn ++ "\n" ++ sh a

isUnderscore :: Ident -> Bool
isUnderscore = eqIdent "_"
