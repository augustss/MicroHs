module MicroHs.Deriving(expandField, deriveHdr, deriveNoHdr, mkGetName) where
import Prelude(); import MHSPrelude
import Data.Char
import Data.Function
import Data.List
import MicroHs.Builtin
import MicroHs.Expr
import MicroHs.Ident
import MicroHs.TCMonad
import Debug.Trace

-- Deriving runs when types level names are resolved, but not value level names.
-- To get access to names that might not be in scope, the module Mhs.Builtin
-- re-exports all names needed here.  This module is automagically imported as B@
-- Generated names should be like
--   type/class names fully qualified
--   method names (on lhs) unqualified
--   constructor names in the derived type unqualified
--   all other names should be qualified with B@

type DeriverT = LHS -> [Constr] -> EConstraint -> T [EDef]
type Deriver = Maybe EConstraint -> DeriverT

derivers :: [(String, Deriver)]
derivers =
  [("Data.Bounded.Bounded",   derBounded)
  ,("Data.Enum.Enum",         derEnum)
  ,("Data.Data.Data",         derData)
  ,("Data.Eq.Eq",             derEq)
  ,("Data.Ix.Ix",             derNotYet)
  ,("Data.Ord.Ord",           derOrd)
  ,("Data.Typeable.Typeable", derTypeable)
  ,("GHC.Generics.Generic",   derNotYet)
  ,("Language.Haskell.TH.Syntax.Lift", derLift)
  ,("Text.Read.Internal.Read",derRead)
  ,("Text.Show.Show",         derShow)
  ]

deriveHdr :: DeriverT
deriveHdr = deriveNoHdr' Nothing

deriveNoHdr :: EConstraint -> DeriverT
deriveNoHdr ctx = deriveNoHdr' (Just ctx)

deriveNoHdr' :: Maybe EConstraint -> DeriverT
deriveNoHdr' mctx lhs cs d = do
  let c = getAppCon d
  case lookup (unIdent c) derivers of
    Nothing -> tcError (getSLoc c) $ "Cannot derive " ++ show c
    Just f  -> f mctx lhs cs d

derNotYet :: Deriver
derNotYet _ _ _ d = do
  notYet d
  return []

notYet :: EConstraint -> T ()
notYet d =
  traceM ("Warning: cannot derive " ++ show d ++ " yet, " ++ showSLoc (getSLoc d))

-- We will never have Template Haskell, but we pretend we can derive Lift for it.
derLift :: Deriver
derLift _ _ _ _ = return []

--------------------------------------------

expandField :: EDef -> T [EDef]
expandField def@(Data    lhs cs _) = (++ [def]) <$> genHasFields lhs cs
expandField def@(Newtype lhs  c _) = (++ [def]) <$> genHasFields lhs [c]
expandField def                    = return [def]

genHasFields :: LHS -> [Constr] -> T [EDef]
genHasFields lhs cs = do
  let fldtys = nubBy ((==) `on` fst) [ (fld, ty) | Constr _ _ _ (Right fs) <- cs, (fld, (_, ty)) <- fs ]
--      flds = map fst fldtys
  concat <$> mapM (genHasField lhs cs) fldtys

genHasField :: LHS -> [Constr] -> (Ident, EType) -> T [EDef]
genHasField (tycon, iks) cs (fld, fldty) = do
  mn <- gets moduleName
  let loc = getSLoc tycon
      qtycon = qualIdent mn tycon
      eFld = EVar fld
      ufld = unIdent fld
      undef = mkExn loc ufld "recSelError"
      iHasField = mkIdentSLoc loc nameHasField
      iSetField = mkIdentSLoc loc nameSetField
      igetField = mkIdentSLoc loc namegetField
      isetField = mkIdentSLoc loc namesetField
      hdrGet = eForall iks $ eApp3 (EVar iHasField)
                                   (ELit loc (LStr ufld))
                                   (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                   fldty
      hdrSet = eForall iks $ eApp3 (EVar iSetField)
                                   (ELit loc (LStr ufld))
                                   (eApps (EVar qtycon) (map (EVar . idKindIdent) iks))
                                   fldty
      conEqnGet (Constr _ _ c (Left ts))   = eEqn [eApps (EVar c) (map (const eDummy) ts)] $ undef
      conEqnGet (Constr _ _ c (Right fts)) = eEqn [conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = eFld
      conEqnSet (Constr _ _ c (Left ts))   = eEqn [eDummy, eApps (EVar c) (map (const eDummy) ts)] $ undef
      conEqnSet (Constr _ _ c (Right fts)) = eEqn [eDummy, conApp] $ if fld `elem` fs then rhs else undef
        where fs = map fst fts
              conApp = eApps (EVar c) (map EVar fs)
              rhs = eLam [eFld] conApp
      getName = mkGetName tycon fld

      -- XXX A hack, we can't handle forall yet.
      validType (EForall _ _ _) = False
      validType _ = True

  pure $ [ Sign [getName] $ eForall iks $ lhsToType (qtycon, iks) `tArrow` fldty
         , Fcn getName $ map conEqnGet cs ]
    ++ if not (validType fldty) then [] else
         [ Instance hdrGet [Fcn igetField [eEqn [eDummy] $ EVar getName] ]
         , Instance hdrSet [Fcn isetField $ map conEqnSet cs]
         ]

nameHasField :: String
nameHasField = "Data.Records.HasField"

nameSetField :: String
nameSetField = "Data.Records.SetField"

namegetField :: String
namegetField = "getField"

namesetField :: String
namesetField = "setField"

mkGetName :: Ident -> Ident -> Ident
mkGetName tycon fld = qualIdent (mkIdent "get") $ qualIdent tycon fld

--------------------------------------------

derTypeable :: Deriver
derTypeable _ (i, _) _ etyp = do
  mn <- gets moduleName
  let
    loc = getSLoc i
    itypeRep  = mkIdentSLoc loc "typeRep"
    imkTyConApp = mkBuiltin loc "mkTyConApp"
    imkTyCon = mkBuiltin loc "mkTyCon"
    hdr = EApp etyp (EVar $ qualIdent mn i)
    mdl = ELit loc $ LStr $ unIdent mn
    nam = ELit loc $ LStr $ unIdent i
    eqns = eEqns [eDummy] $ eAppI2 imkTyConApp (eAppI2 imkTyCon mdl nam) (EListish (LList []))
    inst = Instance hdr [Fcn itypeRep eqns]
  return [inst]

--------------------------------------------

getFieldTys :: (Either [SType] [ConstrField]) -> [EType]
getFieldTys (Left ts) = map snd ts
getFieldTys (Right ts) = map (snd . snd) ts

decomp :: EType -> [EType]
decomp t | Just (c, ts) <- getAppM t, isConIdent c = concatMap decomp ts
         | otherwise = [t]

-- If there is no mctx we use the default strategy to derive the instance context.
-- The default strategy basically extracts all subtypes with variables.
mkHdr :: Maybe EConstraint -> LHS -> [Constr] -> EConstraint -> T EConstraint
mkHdr (Just ctx) _ _ _ = return ctx
mkHdr _ (t, iks) cs cls = do
  mn <- gets moduleName
  let ctys :: [EType]  -- All top level types used by the constructors.
      ctys = nubBy eqEType [ tt | Constr evs _ _ flds <- cs, ft <- getFieldTys flds, tt <- decomp ft,
                            not $ null $ freeTyVars [tt] \\ map idKindIdent evs, not (eqEType ty tt) ]
      ty = tApps (qualIdent mn t) $ map tVarK iks
  pure $ eForall iks $ addConstraints (map (tApp cls) ctys) $ tApp cls ty

mkPat :: Constr -> String -> (EPat, [Expr])
mkPat (Constr _ _ c flds) s =
  let n = either length length flds
      loc = getSLoc c
      vs = map (EVar . mkIdentSLoc loc . (s ++) . show) [1..n]
  in  (tApps c vs, vs)

cannotDerive :: String -> Ident -> EConstraint -> T [EDef]
cannotDerive cls ty e = tcError (getSLoc e) $ "Cannot derive " ++ cls ++ " " ++ show ty

--------------------------------------------

derEq :: Deriver
derEq mctx lhs cs@(_:_) eeq = do
  hdr <- mkHdr mctx lhs cs eeq
  let loc = getSLoc eeq
      mkEqn c =
        let (xp, xs) = mkPat c "x"
            (yp, ys) = mkPat c "y"
        in  eEqn [xp, yp] $ if null xs then eTrue else foldr1 eAnd $ zipWith eEq xs ys
      eqns = map mkEqn cs ++ [eEqn [eDummy, eDummy] eFalse]
      iEq = mkIdentSLoc loc "=="
      eEq = EApp . EApp (EVar $ mkBuiltin loc "==")
      eAnd = EApp . EApp (EVar $ mkBuiltin loc "&&")
      eTrue = EVar $ mkBuiltin loc "True"
      eFalse = EVar $ mkBuiltin loc "False"
      inst = Instance hdr [Fcn iEq eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derEq _ (c, _) _ e = cannotDerive "Eq" c e

--------------------------------------------

derOrd :: Deriver
derOrd mctx lhs cs@(_:_) eord = do
  hdr <- mkHdr mctx lhs cs eord
  let loc = getSLoc eord
      mkEqn c =
        let (xp, xs) = mkPat c "x"
            (yp, ys) = mkPat c "y"
        in  [eEqn [xp, yp] $ if null xs then eEQ else foldr1 eComb $ zipWith eCompare xs ys
            ,eEqn [xp, eDummy] $ eLT
            ,eEqn [eDummy, yp] $ eGT]
      eqns = concatMap mkEqn cs
      iCompare = mkIdentSLoc loc "compare"
      eCompare = EApp . EApp (EVar $ mkBuiltin loc "compare")
      eComb = EApp . EApp (EVar $ mkBuiltin loc "<>")
      eEQ = EVar $ mkBuiltin loc "EQ"
      eLT = EVar $ mkBuiltin loc "LT"
      eGT = EVar $ mkBuiltin loc "GT"
      inst = Instance hdr [Fcn iCompare eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derOrd _ (c, _) _ e = cannotDerive "Ord" c e

--------------------------------------------

derBounded :: Deriver
derBounded mctx lhs cs@(c0:_) ebnd = do
  hdr <- mkHdr mctx lhs cs ebnd
  let loc = getSLoc ebnd
      mkEqn bnd (Constr _ _ c flds) =
        let n = either length length flds
        in  eEqn [] $ tApps c (replicate n (EVar bnd))

      iMinBound = mkIdentSLoc loc "minBound"
      iMaxBound = mkIdentSLoc loc "maxBound"
      minEqn = mkEqn iMinBound c0
      maxEqn = mkEqn iMaxBound (last cs)
      inst = Instance hdr [Fcn iMinBound [minEqn], Fcn iMaxBound [maxEqn]]
  -- traceM $ showEDefs [inst]
  return [inst]
derBounded _ (c, _) _ e = cannotDerive "Bounded" c e

--------------------------------------------

derEnum :: Deriver
derEnum mctx lhs cs@(_:_) enm | all isNullary cs = do
  hdr <- mkHdr mctx lhs cs enm
  let loc = getSLoc enm

      mkFrom (Constr _ _ c _) i =
        eEqn [EVar c] $ ELit loc (LInt i)
      mkTo (Constr _ _ c _) i =
        eEqn [ELit loc (LInt i)] $ EVar c

      iFromEnum = mkIdentSLoc loc "fromEnum"
      iToEnum = mkIdentSLoc loc "toEnum"
      fromEqns = zipWith mkFrom cs [0..]
      toEqns   = zipWith mkTo   cs [0..]
      inst = Instance hdr [Fcn iFromEnum fromEqns, Fcn iToEnum toEqns]
  --traceM $ showEDefs [inst]
  return [inst]
derEnum _ (c, _) _ e = cannotDerive "Enum" c e

isNullary :: Constr -> Bool
isNullary (Constr _ _ _ flds) = either null null flds

--------------------------------------------

derShow :: Deriver
derShow mctx lhs cs@(_:_) eshow = do
  hdr <- mkHdr mctx lhs cs eshow
  let loc = getSLoc eshow
      mkEqn c@(Constr _ _ nm flds) =
        let (xp, xs) = mkPat c "x"
        in  eEqn [varp, xp] $ showRHS nm xs flds

      var = EVar . mkBuiltin loc
      varp = EVar $ mkIdent "p"
      lit = ELit loc

      iShowsPrec = mkIdentSLoc loc "showsPrec"
      eShowsPrec n = eApp2 (var "showsPrec") (lit (LInt n))
      eShowString s = EApp (var "showString") (lit (LStr s))
      eParen n = eApp2 (var "showParen") (eApp2 (var ">") varp (lit (LInt n)))
      eShowL s = foldr1 ejoin . intersperse (eShowString s)
      ejoin = eApp2 (var ".")

      showRHS nm [] _ = eShowString (unIdentPar nm)
      showRHS nm xs (Left   _) = showRHSN nm xs
      showRHS nm xs (Right fs) = showRHSR nm $ zip (map fst fs) xs

      showRHSN nm xs = eParen 10 $ eShowL " " $ eShowString (unIdentPar nm) : map (eShowsPrec 11) xs

      showRHSR nm fxs =
        eShowString (unIdentPar nm ++ "{") `ejoin`
        (eShowL "," $ map fld fxs) `ejoin`
        eShowString "}"
          where fld (f, x) = eShowString (unIdentPar f ++ "=") `ejoin` eShowsPrec 0 x

      eqns = map mkEqn cs
      inst = Instance hdr [Fcn iShowsPrec eqns]
--  traceM $ showEDefs [inst]
  return [inst]
derShow _ (c, _) _ e = cannotDerive "Show" c e

unIdentPar :: Ident -> String
unIdentPar i =
  let s = unIdent i
  in  if isAlpha (head s) then s else "(" ++ s ++ ")"

--------------------------------------------

-- Deriving for the fake Data class.
derData :: Deriver
derData mctx lhs cs edata = do
  notYet edata
  hdr <- mkHdr mctx lhs cs edata
  let
    inst = Instance hdr []
  return [inst]

--------------------------------------------

derRead :: Deriver
derRead mctx lhs cs eread = do
  notYet eread
  hdr <- mkHdr mctx lhs cs eread
  let
    loc = getSLoc eread
    iReadPrec = mkIdentSLoc loc "readPrec"
    err = eEqn [] $ EApp (EVar $ mkBuiltin loc "error") (ELit loc (LStr "readPrec not defined"))
    inst = Instance hdr [Fcn iReadPrec [err]]
  return [inst]

--------------------------------------------

