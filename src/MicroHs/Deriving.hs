module MicroHs.Deriving(expandField, doDeriving) where
import Prelude
import Data.Function
import Data.List
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.TCMonad

expandField :: EDef -> T [EDef]
expandField def@(Data    lhs cs _) = (def:) <$> genHasFields lhs cs
expandField def@(Newtype lhs  c _) = (def:) <$> genHasFields lhs [c]
expandField def                    = return [def]

genHasFields :: LHS -> [Constr] -> T [EDef]
genHasFields lhs cs = do
  let fldtys = nubBy ((==) `on` fst) [ (fld, ty) | Constr _ _ _ (Right fs) <- cs, (fld, (_, ty)) <- fs ]
--      flds = map fst fldtys
  mapM (genHasField lhs cs) fldtys

genHasField :: LHS -> [Constr] -> (Ident, EType) -> T EDef
genHasField (tycon, iks) cs (fld, fldty) = do
  mn <- gets moduleName
  let loc = getSLoc tycon
      qtycon = qualIdent mn tycon
      dum = EVar dummyIdent
      eFld = EVar fld
      undef = EVar $ mkIdentSLoc loc "undefined"  -- XXX could be nicer
      iHasField = mkIdentSLoc loc nameHasField
      ihasField = mkIdentSLoc loc namehasField
      hdr = eForall iks $ eApp3 (EVar iHasField)
                                  (ELit loc (LStr (unIdent fld)))
                                  (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                  fldty
      conEqn (Constr _ _ c (Left ts))   = eEqn [dum, eApps (EVar c) (map (const dum) ts)] $ undef
      conEqn (Constr _ _ c (Right fts)) = eEqn [dum, conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = ETuple [eFld, eLam [eFld] conApp]
  pure $ Instance hdr [BFcn ihasField $ map conEqn cs]

eApp2 :: Expr -> Expr -> Expr -> Expr
eApp2 a b c = EApp (EApp a b) c

eApp3 :: Expr -> Expr -> Expr -> Expr -> Expr
eApp3 a b c d = EApp (eApp2 a b c) d

eApps :: Expr -> [Expr] -> Expr
eApps = foldl EApp

eForall :: [IdKind] -> EType -> EType
eForall [] t = t
eForall vs t = EForall vs t

nameHasField :: String
nameHasField = "Data.Record.HasField"

namehasField :: String
namehasField = "hasField"

--------------------------------------------

doDeriving :: EDef -> T [EDef]
doDeriving def@(Data    lhs cs ds) = (def:) . concat <$> mapM (derive lhs  cs) ds
doDeriving def@(Newtype lhs  c ds) = (def:) . concat <$> mapM (derive lhs [c]) ds
doDeriving def                     = return [def]

derive :: LHS -> [Constr] -> EConstraint -> T [EDef]
derive _lhs _cs _con = return []
