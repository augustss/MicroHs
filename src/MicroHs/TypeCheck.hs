{-# LANGUAGE QualifiedDo #-}
module MicroHs.TypeCheck(module MicroHs.TypeCheck) where
import Prelude
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import Control.Monad.State as T
import qualified MicroHs.StringMap as M
import MicroHs.Parse
--import Compat

data TModule a = TModule IdentModule [TypeExport] [ValueExport] a

data TypeExport = TypeExport Ident Ident TypeInfo  -- exported name, original name
  --Xderiving (Show)

data ValueExport = ValueExport Ident Ident ETypeScheme
  --Xderiving (Show)

typeCheck :: [(ImportSpec, TModule a)] -> EModule -> TModule EModule
typeCheck imps amdl =
  let (vs, ts) = mkTables imps
  in  case amdl of
        EModule mn es ds ->
          case runState (tcDefs ds) (initTC vs ts) of
            (tds, _) ->
              TModule mn undefined undefined $ EModule mn es tds

mkTables :: [(ImportSpec, TModule a)] -> (ValueTable, TypeTable)
mkTables mdls =
  let
    qns aisp mn i =
      case aisp of
        ImportSpec q _ mas ->
          let
            m = fromMaybe mn mas
          in  if q then [qual m i] else [i, qual m i]
    --XallValues :: ValueTable
    allValues =
      let
        syms arg =
          case arg of
            (is, TModule mn _ ves _) -> [ (v, [Entry qi t]) | ValueExport i qi t <- ves, v <- qns is mn i ]
      in  M.fromListWith (unionBy eqEntry) $ concatMap syms mdls
    --XallTypes :: TypeTable
    allTypes =
      let
        types arg =
          case arg of
            (is, TModule mn tes _ _) -> [ (v, [Entry qi (kindOf ti)]) | TypeExport i qi ti <- tes, v <- qns is mn i ]
      in M.fromListWith (unionBy eqEntry) $ concatMap types mdls
  in  (allValues, allTypes)

kindOf :: TypeInfo -> ETypeScheme
kindOf ti =
  case ti of
    TAbs k -> ETypeScheme [] k
    TConc k _ -> ETypeScheme [] k

eqEntry :: Entry -> Entry -> Bool
eqEntry x y =
  case x of
    Entry ix _ ->
      case y of
        Entry iy _ -> eqIdent ix iy

--------------------------

type Typed a = (a, EType)

data Entry = Entry Ident ETypeScheme
  --Xderiving(Show)

type ValueTable = M.Map [Entry]
type TypeTable  = M.Map [Entry]

data TCState = TC Int TypeTable ValueTable (IM.IntMap EType)
  --Xderiving (Show)

typeTable :: TCState -> TypeTable
typeTable ts =
  case ts of
    TC _ tt _ _ -> tt

valueTable :: TCState -> ValueTable
valueTable ts =
  case ts of
    TC _ _ vt _ -> vt

initTC :: TypeTable -> ValueTable -> TCState
initTC ts vs = TC 1 ts vs IM.empty

type T a = State TCState a

tCon :: Ident -> EType
tCon = EVar

tApps :: Ident -> [EType] -> EType
tApps i ts = foldl EApp (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = EApp (EApp (EVar "->") a) r

munify :: Maybe EType -> EType -> T ()
munify mt b =
  case mt of
    Nothing -> T.return ()
    Just a -> unify a b

unify :: EType -> EType -> T ()
unify _a _b = undefined

unMType :: Maybe EType -> T EType
unMType mt =
  case mt of
    Nothing -> newUVar
    Just t -> T.return t

newUVar :: T EType
newUVar = T.do
  TC n tenv venv sub <- get
  put (TC (n+1) tenv venv sub)
  T.return (EUVar n)

tLookupInst :: Ident -> T (Ident, EType)
tLookupInst i = T.do
  (qi, s) <- tLookup i
  t <- tInst s
  return (qi, t)

tLookup :: Ident -> T (Ident, ETypeScheme)
tLookup i = T.do
  env <- gets valueTable
  case M.lookup i env of
    Nothing -> error $ "undefined variable " ++ i
    Just aes ->
      case aes of
        [] -> undefined
        Entry qi s : es ->
          if null es then
            T.return (qi, s)
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

extVal :: Ident -> ETypeScheme -> T a -> T a
extVal i t ta = T.do
  old <- get
  case old of
    TC n tenv venv sub -> T.do
      put (TC n tenv (M.insert i [Entry i t] venv) sub)
      a <- ta
      TC nn _ _ nsub <- get
      put (TC nn tenv venv nsub)
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
tcDefs ds = undefined

tcExpr :: Maybe EType -> Expr -> T (Typed Expr)
tcExpr mt ae =
  case ae of
    EVar i -> T.do
      (qi, t) <- tLookupInst i
      munify mt t
      T.return (EVar qi, t)
    EApp f a -> T.do
      (ea, ta) <- tcExpr Nothing a
      tr <- unMType mt
      (ef, _) <- tcExpr (Just (tArrow ta tr)) f
      T.return (EApp ea ef, tr)
    ELam i e -> T.do
      ta <- newUVar
      (ee, tr) <- extVal i (ETypeScheme [] ta) $ tcExpr Nothing e
      let
        tlam = tArrow ta tr
      munify mt tlam
      T.return (ELam i ee, tlam)
    EInt _ -> T.return (ae, tCon "Data.Int.Int")
    EChar _ -> T.return (ae, tCon "Data.Char.Char")
    EStr _ -> T.return (ae, tApps "Data.List.[]" [tCon "Data.Char.Char"])
    ECase _ _ -> undefined
    ELet _ _ -> undefined
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
    ESectL e i -> tcExpr mt (EApp (EVar i) e)
    ESectR _ _ -> undefined
    EIf e1 e2 e3 -> T.do
      (ee1, _) <- tcExpr (Just (tCon "Data.Bool.Bool")) e1
      (ee2, te2) <- tcExpr mt e2
      (ee3, te3) <- tcExpr mt e3
      unify te2 te3
      T.return (EIf ee1 ee2 ee3, te2)
    ECompr _ _ -> undefined
    EBad -> undefined    -- shouldn't happen
    EUVar _ -> undefined -- shouldn't happen
    ECaseT _ _ _ -> undefined -- shouldn't happen
