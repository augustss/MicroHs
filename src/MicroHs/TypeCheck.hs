{-# LANGUAGE QualifiedDo #-}
module MicroHs.TypeCheck(module MicroHs.TypeCheck) where
import Control.Monad.State as T
import Parse

type Typed a = (a, EType)

tCon :: Ident -> EType
tCon = EVar

tApps :: Ident -> [EType] -> EType
tApps i ts = foldl EApp (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = EApp (EApp (EVar "->") a) r

munify :: Maybe EType -> EType T ()
munify mt b =
  case mt of
    Nothing -> return ()
    Just a -> unify a b

unify :: EType -> EType -> T ()
unify _a _b = undefined

unMType :: Maybe EType -> T EType
unMType mt =
  case mt of
    Nothing -> newUVar
    Just t -> return t

newUVar :: T EType
newUVar = undefined

{-
unMTypeArrow :: Maybe EType -> T (EType, Maybe EType)
unMTypeArrow mt =
  case mt of
    Nothing -> T.do
      t <- newUVar
      return (t, Nothing)
    Just tarr -> T.do
      case getArrow tarr of
        Just (ta, tr) -> (ta, Just tr)
	Nothing -> T.do
-}

tcExpr :: Maybe EType -> Expr -> T (Typed Expr)
tcExpr mt ae =
  case ae of
    EVar i -> T.do
      t <- tLookupInst i
      munify mt t
      return (ae, t)
    EApp f a -> T.do
      (ea, ta) <- tcExpr Nothing a
      tr <- unMType mt
      (ef, _) <- tcExpr (Just (tArrow ta tr)) f
      return (EApp ea ef, tr)
    ELam i e -> T.do
      ta <- newUVar
      (ee, tr) <- extVals i ta $ tcExpr Nothing e
      let
        tlam = tArrow ta tr
      munify mt tlam
      return (ELam i ee, tlam)
    EInt _ -> return (ea, tCon "Data.Int.Int")
    EChar _ -> return (ea, tCon "Data.Char.Char")
    EStr _ -> return (ea, tApps "Data.List.[]") [tCon "Data.Char.Char"]
    ECase _ -> undefined
    ELet _ -> undefined
    ETuple es -> T.do
      let
        n = length es
      (ees, tes) <- unzip <$> zipWithM tcExpr mts es
      let
        ttup = tApps (tupleConstr n) tes
      munify mt ttup
      return (ETuple ees, ttup)
    EList es -> T.do
      met <- unMTypeList mt
      (ees, _) <- unzip <$> mapM_ (tcExpr met) es
      return (EList ees, unMType mt)
    EDo _ -> undefined
    EPrim p -> return (ea, lookupPrimType p)
    ESectL _ _ -> undefined
    ESectR _ _ -> undefined
    EIf e1 e2 e3 -> do
      (ee1, _) <- tcExpr (Just (tCon "Data.Bool.Bool")) e1
      (ee2, te2) <- tcExpr mt e2
      (ee3, te3) <- tcExpr mt e3
      unify te2 te3
      return (EIf ee1 ee2 ee3, te2)
