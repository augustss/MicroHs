module MicroHs.Deriving(expandField, doDeriving) where
import Prelude
import Control.Monad
import Data.Function
import Data.List
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.TCMonad
import Debug.Trace

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
      eFld = EVar fld
      undef = EVar $ mkIdentSLoc loc "undefined"  -- XXX could be nicer
      iHasField = mkIdentSLoc loc nameHasField
      ihasField = mkIdentSLoc loc namehasField
      hdr = eForall iks $ eApp3 (EVar iHasField)
                                  (ELit loc (LStr (unIdent fld)))
                                  (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                  fldty
      conEqn (Constr _ _ c (Left ts))   = eEqn [dummy, eApps (EVar c) (map (const dummy) ts)] $ undef
      conEqn (Constr _ _ c (Right fts)) = eEqn [dummy, conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = ETuple [eFld, eLam [eFld] conApp]
  pure $ Instance hdr [BFcn ihasField $ map conEqn cs]

dummy :: Expr
dummy = EVar dummyIdent

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

type Deriver = LHS -> [Constr] -> EConstraint -> T [EDef]

derivers :: [(String, Deriver)]
derivers =
  [("Data.Typeable.Typeable", derTypeable)
  ,("Data.Eq.Eq",             derNotYet)
  ,("Data.Ord.Ord",           derNotYet)
  ,("Text.Show.Show",         derNotYet)
  ]

derive :: Deriver
derive lhs cs d = do
  let c = getAppCon d
  case lookup (unIdent c) derivers of
    Nothing -> tcError (getSLoc c) $ "Cannot derive " ++ show c
    Just f  -> f lhs cs d

derNotYet :: Deriver
derNotYet _ _ d = do
  when False $ traceM ("Warning: cannot derive " ++ show d ++ " yet")
  return []

derTypeable :: Deriver
derTypeable (i, _) _ etyp = do
  mn <- gets moduleName
  let
    loc = getSLoc i
    itypeRep  = mkIdentSLoc loc "typeRep"
    imkTyConApp = mkIdentSLoc loc "mkTyConApp"
    imkTyCon = mkIdentSLoc loc "mkTyCon"
    hdr = EApp etyp (EVar $ qualIdent mn i)
    mdl = ELit loc $ LStr $ unIdent mn
    nam = ELit loc $ LStr $ unIdent i
    eqns = eEqns [dummy] $ eApp2 (EVar imkTyConApp) (eApp2 (EVar imkTyCon) mdl nam) (EVar (mkIdent "[]"))
    inst = Instance hdr [BFcn itypeRep eqns]
  return [inst]
