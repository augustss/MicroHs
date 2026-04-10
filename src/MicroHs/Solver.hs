-- This implementation closely follows
--   "Type checking with open type functions"
--   https://dl.acm.org/doi/10.1145/1411204.1411215
-- The situation here is simpler since we don't have to
-- generate any evidence of the equality.

module MicroHs.Solver(completeLocal) where
import Control.Applicative
import Control.Monad
import Data.Maybe
import MicroHs.Expr
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.State
import MicroHs.TCMonad
import Debug.Trace
import Text.PrettyPrint.HughesPJLiteClass

data LocalEq = EType :~: EType
  deriving (Show)
instance Eq LocalEq where
  (a :~: b) == (a' :~: b')  =  eqEType a a' && eqEType b b'
instance Pretty LocalEq where
  pPrintPrec l _ (a :~: b) = pPrint0 l a <+> text ":~:" <+> pPrint0 l b

--type LocalEq    = (EType, EType)     -- local, i.e., given equality, t1 ~ t2
type LocalEqs   = [LocalEq]
type GlobalFams = M.Map TypeFamInfo  -- globally defined type families
type SkolemMap  = M.Map EType

type S a = State SolverState a

data SolverState = SolverState {
  locals     :: LocalEqs,            -- equalities being transformed
  nextSkolem :: Int,                 -- generate a new skolem
  skolemMap  :: SkolemMap,           -- and map the skolems
  globals    :: GlobalFams           -- fixed set of global equalities
  }
  deriving (Show)

modLocals :: (LocalEqs -> LocalEqs) -> S ()
modLocals f = modify $ \ s -> s{ locals = f (locals s) }

modLocalsMap :: (LocalEq -> LocalEq) -> S ()
modLocalsMap f = modify $ \ s -> s{ locals = map f (locals s) }

modLocalsConcatMap :: (LocalEq -> [LocalEq]) -> S ()
modLocalsConcatMap f = modify $ \ s -> s{ locals = concatMap f (locals s) }

type Skolem = Ident

skolemPrefix :: Char
skolemPrefix = '?'

newSkolem :: S Skolem
newSkolem = do
  s <- get
  let n = nextSkolem s
  put s{ nextSkolem = n + 1 }
  pure $ mkIdent (skolemPrefix : show n)

isSkolemIdent :: Skolem -> Bool
isSkolemIdent i = head (unIdent i) == skolemPrefix

isSkolem :: EType -> Bool
isSkolem (EVar i) = isSkolemIdent i
isSkolem _ = False

isData :: GlobalFams -> EType -> Bool
isData fams = maybe False (\ (i, _) -> isNothing (M.lookup i fams) && isConIdent i) . getAppM

isFam :: GlobalFams -> EType -> Bool
isFam fams = maybe False (\ (i, _) -> isJust (M.lookup i fams)) . getAppM

isTyVar :: EType -> Bool
isTyVar (EVar i) = not (isConIdent i || isSkolemIdent i)
isTyVar _ = False

-- Complete a set of given type equalities,
-- returning a the completeed set and a skolem map.
completeLocal :: [(EType, EType)] -> T (LocalEqs, SkolemMap)
completeLocal alcl = do
  let lcl = map (\ (a,b) -> a :~: b) alcl
  n <- gets unique
  tf <- gets typeFamTable
  let complete = fixS $ trivS >> swapS >> skolemS >> decompS >> topS >> failS >> substS
      initS = SolverState{ nextSkolem = n, globals = tf, skolemMap = M.empty, locals = lcl }
  case execState complete initS of
    SolverState{ nextSkolem = ns, locals = l, skolemMap = sm, globals = g } -> do
      modify $ \ s -> s{ unique = ns }
      checkLocalInvariant g l
      return (l, sm)

checkLocalInvariant :: GlobalFams -> LocalEqs -> T ()
checkLocalInvariant g es | all ok es = pure ()
                         | otherwise = error "checkLocalInvariant"
  where ok (t1 :~: t2) | t1 `subTerm` t2 = False
        ok (t :~: _) = isTyVar t || isFam g t

trc :: String -> S () -> S ()
trc _ act = act
{-
trc s act = do
  l <- gets locals
  act
  l' <- gets locals
  when (l /= l') $
    traceM $ s ++ ": " ++ prettyShow l ++ " --> " ++ prettyShow l'
-}

fixS :: S () -> S ()
fixS step = trc "fixS" $ do
  s <- gets locals
  step
  s' <- gets locals
  if s == s' then
    pure ()
  else
    fixS step

trivS :: S ()
trivS = trc "trivS" $ modLocals $ filter (\ (t1 :~: t2) -> not (eqEType t1 t2))

swapS :: S ()
swapS = funSwapS >> varSwapS >> alphaSwapS

funSwapS :: S ()
funSwapS = trc "funSwapS" $ do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1 :~: t2) | isFam g t2 && (isData g t1 || isSkolem t1 || t1 `subTerm` t2) = (t2 :~: t1)
      f tt = tt
  modLocalsMap f

varSwapS :: S ()
varSwapS = trc "varSwapS" $ do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1 :~: t2) | (isData g t1 || isSkolem t1) && isTyVar t2 = (t2 :~: t1)
      f tt = tt
  modLocalsMap f

alphaSwapS :: S ()
alphaSwapS = trc "alphaSwapS" $ do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1 :~: t2) | isData g t1 && isSkolem t2 = (t2 :~: t1)
      f tt = tt
  modLocalsMap f

skolemS :: S ()
skolemS = trc "skolemS" $ do
  g <- gets globals
  let one (t :~: cft) | not (isData g t), isData g cft,
                        Just ft <- findFamWith t cft = do
        sk <- newSkolem
        let a = EVar sk
            ca = doSubstTy (ft :~: a) cft
            fca = doSubstTy (t :~: ca) ft
        modify $ \ s -> s{ skolemMap = M.insert sk ft (skolemMap s) }
        pure [ (t :~: doSubstTy (ft :~: a) cft), (a :~: fca) ]
      one eq = pure [eq]
      findFamWith :: EType -> EType -> Maybe EType
      findFamWith p t | isFam g t, p `subTerm` t = Just t
      findFamWith p (EApp f a) = findFamWith p f <|> findFamWith p a
      findFamWith _ (EVar _) = Nothing
      findFamWith _ t = impossiblePP t
  lcl <- gets locals
  lcl' <- concat <$> mapM one lcl
  modLocals (const lcl')

decompS :: S ()
decompS = trc "decompS" $ do
  g <- gets globals
  let f :: LocalEq -> [LocalEq]
      f (t1 :~: t2) | isData g t1,
                      Just (i1, ts1) <- getAppM t1,
                      Just (i2, ts2) <- getAppM t2,
                      i1 == i2 = zipWith (:~:) ts1 ts2
      f tt = [tt]
  modLocalsConcatMap f

topS :: S ()
topS = trc "topS" $ do
  g <- gets globals
  let f t | Just t' <- applyFam g t = t'
      f (EApp t1 t2) = EApp (f t1) (f t2)
      f t = t
  modLocalsMap $ \ (t1 :~: t2) -> (f t1 :~: f t2)

-- We will get an error sooner or later. ?
failS :: S ()
failS = pure () -- decompFailS >> occursCheckS

substS :: S ()
substS = do
  fixS topS
  fixS substLRS

substLRS :: S ()
substLRS = trc "substS" $ do
  g <- gets globals
  let one r [] = reverse r
      one r (eq:eqs) | okEq eq   = one (eq : subEqs eq r) (subEqs eq eqs)
                     | otherwise = one (eq : r) eqs
      subEqs eq = map (\ (t1 :~: t2) -> doSubstTy eq t1 :~: doSubstTy eq t2)
      okEq (t :~: s) = not (s `subTerm` t) && not (isData g t)
  modLocals (one [])

doSubstTy :: LocalEq -> EType -> EType
doSubstTy (t :~: s) u | t `eqEType` u = s
doSubstTy eq (EApp f a) = EApp (doSubstTy eq f) (doSubstTy eq a)
doSubstTy _ v@(EVar _) = v
doSubstTy _ v = impossiblePP v

subTerm :: EType -> EType -> Bool
subTerm t s | eqEType t s = True
subTerm t (EApp f a) = t `subTerm` f || t `subTerm` a
subTerm _ (EVar _) = False
subTerm _ e = impossiblePP e

applyFam :: GlobalFams -> EType -> Maybe EType
applyFam fams t | Just (i, ts) <- getAppM t,
                  Just tfi <- M.lookup i fams =
                    let loc = getSLoc t in
                      case matchFam loc tfi ts of
                        Right mt -> mt
                        Left s   -> errorMessage loc s
applyFam _ _ = Nothing

-- XXX Not using FDs
matchFam :: SLoc -> TypeFamInfo -> [EType] -> Either String (Maybe EType)
matchFam loc tfi as =
  let las = length as
      red :: TFEqn -> Maybe EType
      red (ps, rhs) | lps > las = Nothing
                    | otherwise = do
                      let (as', as'') = splitAt lps as
                      let xxx :: [Maybe (TySubst, [Improve])]
                          xxx = map (matchTypesFD loc ps as') (tfIFunDeps tfi)
                      (s, _imp) <- asum $ xxx
                      pure (eApps (substEUVar s rhs) as'')
        where lps = length ps
--  traceM $ "redTypeFam' " ++ show (f, tfi, tfEqns tfi, map red (tfEqns tfi))
  in  case mapMaybe red (tfEqns tfi) of
        [] -> Right Nothing
        t : xs | tfAper tfi == TFClosedSet || null xs -> Right (Just t)
        _ -> Left "Multiple matching type family equations"        

--------------
-- temporary copy from TypeCheck.hs

type Improve = (SLoc, EType, EType)  -- Unify to get an improvement substitution
matchTypesFD :: SLoc -> [EType] -> [EType] -> IFunDep -> Maybe (TySubst, [Improve])
--matchTypesFD _ ts ts' io | trace ("matchTypesFD: " ++ show (ts, ts', io)) False = undefined
matchTypesFD loc ts ts' (ins, outs) = do
  let matchFD :: Bool -> EType -> EType -> Maybe TySubst
      matchFD True  = \ _ _ -> Just []     -- if it's an output, don't match
      matchFD False = matchType []         -- match types for non-outputs
  tms <- sequence $ zipWith3 matchFD outs ts ts'
  tm  <- combineTySubsts tms               -- combine all substitutions
  is  <- combineTySubsts [ s | (True, s) <- zip ins tms]  -- subst from input FDs
  let imp = [ (loc, substEUVar is t, t') | (True, t, t') <- zip3 outs ts ts' ]  -- improvements
  pure (tm, imp)

