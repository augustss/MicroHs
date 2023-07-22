{-# LANGUAGE QualifiedDo #-}
module MicroHs.TypeCheck(module MicroHs.TypeCheck) where
import Control.Monad.State as T
import Parse

data Typed a = Typed EType a

tCon :: Ident -> EType
tCon = EVar

tApps :: Ident -> [EType] -> EType
tApps i ts = foldl EApp (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = EApp (EApp (EVar "->") a) r

tcExpr :: Maybe EType -> Expr -> T (Typed Expr)
tcExpr mt ae =
  let
    ret t = TExpr t ae
  in
    case ae of
      EVar i -> T.do
        t <- tLookup i
        munify mt t
        ret t
      EApp f a -> T.do
        ta <- tcExpr Nothing a
        tr <- unMType mt
        tcExpr (Just (tArrow ta tr)) f
        ret tr
      ELam i e -> T.do
        (ta, mtr) <- unMTypeArrow mt
        tr <- ext t ta $ tcExpr mtr e
        ret (tArrow ta tr)
      EInt _ -> ret (tCon "Data.Int.Int")
      EChar _ -> ret (tCon "Data.Char.Char")
      EStr _ -> ret (tApps "Data.List.[]") [tCon "Data.Char.Char"]
      ECase _ -> undefined
      ELet _ -> undefined
      ETuple es -> T.do
        let
          n = length es
        mts <- unMTypeTuple n mt
        trs <- zipWithM tcExpr mts es
        ret (tApps (tupleConstr n) trs)
      EList es -> T.do
        met <- unMTypeList mt
        mapM_ (tcExpr met) es
        ret (unMType mt)
      EDo _ -> undefined
      EPrim p -> ret (lookupPrimType p)
      ESectL _ _ -> undefined
      ESectR _ _ -> undefined
      EIf e1 e2 e3 -> do
        tcExpr (Just (tCon "Data.Bool.Bool")) e1
        tc
