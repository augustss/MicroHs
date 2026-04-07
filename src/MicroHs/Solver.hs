module MicroHs.Solver(completeLocal) where
import Data.Maybe
import MicroHs.Expr
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.State
import MicroHs.TCMonad

type LocalEq    = (EType, EType)     -- local, i.e., given equality, t1 ~ t2
type LocalEqs   = [LocalEq]          
type GlobalFams = M.Map TypeFamInfo  -- globally defined type families
type SkolemMap  = M.Map EType

type S a = State SolverState a

data SolverState = SolverState {
  locals     :: LocalEqs,
  nextSkolem :: Int,
  globals    :: GlobalFams,
  skolemMap  :: SkolemMap
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
isData fams = maybe False (\ (i, _) -> isNothing (M.lookup i fams)) . getAppM

isFam :: GlobalFams -> EType -> Bool
isFam fams = maybe False (\ (i, _) -> isJust (M.lookup i fams)) . getAppM

isTyVar :: EType -> Bool
isTyVar (EVar i) = not (isConIdent i || isSkolemIdent i)
isTyVar _ = False

-- Complete a set of given type equalities,
-- returning a the completeed set and a skolem map.
-- This is taken from
--   "Type checking with open type functions"
--   https://dl.acm.org/doi/10.1145/1411204.1411215
-- The situation here is simpler since we don't have to
-- generate any evidence of the equality.
completeLocal :: LocalEqs -> T (LocalEqs, SkolemMap)
completeLocal lcl = do
  n <- gets unique
  tf <- gets typeFamTable
  let complete = fixS $ trivS >> swapS >> skolemS >> decompS >> topS >> failS >> substS
      initS = SolverState{ nextSkolem = n, globals = tf, skolemMap = M.empty, locals = lcl }
  case execState complete initS of
    SolverState{ nextSkolem, locals, skolemMap } -> do
      modify $ \ s -> s{ unique = nextSkolem }
      return (locals, skolemMap)

fixS :: S () -> S ()
fixS step = do
  s <- gets locals
  step
  s' <- gets locals
  if eqTypes s s' then
    pure ()
  else
    fixS step
 where
  -- YUK!
  eqTypes as bs =
    let (asl, asr) = unzip as
        (bsl, bsr) = unzip bs
    in  length as == length bs &&
        and (zipWith eqEType asl bsl) &&
        and (zipWith eqEType asr bsr)

trivS :: S ()
trivS = modLocals $ filter (\ (t1, t2) -> not (eqEType t1 t2))

swapS :: S ()
swapS = funSwapS >> varSwapS >> alphaSwapS

funSwapS :: S ()
funSwapS = do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1, t2) | (isData g t1 || isSkolem t1) && isFam g t2 = (t2, t1)
      f tt = tt
  modLocalsMap f

varSwapS :: S ()
varSwapS = do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1, t2) | (isData g t1 || isSkolem t1) && isTyVar t2 = (t2, t1)
      f tt = tt
  modLocalsMap f

alphaSwapS :: S ()
alphaSwapS = do
  g <- gets globals
  let f :: LocalEq -> LocalEq
      f (t1, t2) | isData g t1 && isSkolem t2 = (t2, t1)
      f tt = tt
  modLocalsMap f

skolemS :: S ()
skolemS = undefined

decompS :: S ()
decompS = do
  g <- gets globals
  let f :: LocalEq -> [LocalEq]
      f (t1, t2) | Just (i1, ts1) <- getAppM t1, Just (i2, ts2) <- getAppM t2,
                   i1 == i2, isData g t1 = zip ts1 ts2
      f tt = [tt]
  modLocalsConcatMap f

topS :: S ()
topS = do
  g <- gets globals
  let f t | Just t' <- applyFam g t = t'
      f (EApp t1 t2) = EApp (f t1) (f t2)
      f t = t
  modLocalsMap $ \ (t1, t2) -> (f t1, f t2)

failS :: S ()
failS = undefined

substS :: S ()
substS = undefined

applyFam :: GlobalFams -> EType -> Maybe EType
applyFam fams t | Just (i, ts) <- getAppM t,
                  Just tfi <- M.lookup i fams =
                  case matchFam tfi ts of
                    Right mt -> mt
                    Left s   -> errorMessage (getSLoc t) s
applyFam _ _ = Nothing

matchFam :: TypeFamInfo -> [EType] -> Either String (Maybe EType)
matchFam = undefined

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

