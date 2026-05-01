module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportItem(..),
  ImportSpec(..),
  ImportItem(..),
  ImpType(..),
  EDef(..), showEDefs,
  Deriving(..), DerStrategy(..), doNotDerive,
  Expr(..), eLam, eLamWithSLoc, eEqn, oneAlt, eEqns, showExpr, eqExpr,
  XTCState,
  CallConv(..),
  QForm(..),
  Listish(..),
  Lit(..), showLit,
  ImpEnt(..), ImpVal(..),
  CType(..),
  EBind, showEBind, showEBinds,
  Eqn(..),
  EStmt(..),
  EAlts(..),
  EField(..), unEField,
  EAlt,
  ECaseArm,
  FunDep,
  EType, showEType, eqEType,
  EConstraint,
  EPat, patVars, isPConApp,
  EKind, kType, kConstraint,
  ESort, sKind,
  IdKind(..), idKindIdent,
  LHS,
  Constr(..), ConstrField, SType,
  ConTyInfo,
  Con(..), conIdent, conArity, conFields,
  tupleConstr, getTupleConstr, getExprTuple,
  mkTupleSel,
  eAppI, eApp2, eAppI2, eApp3, eAppI3, eApps,
  eLetB,
  lhsToType,
  subst, allBinders,
  allVarsExpr, allVarsBind, allVarsEqns, allVarsPat,
  setSLocExpr,
  errorMessage,
  Assoc(..), Fixity,
  getBindsVars,
  getStmtBound,
  HasLoc(..),
  eForall, eForall', unForall,
  eDummy,
  impossible, impossibleShow, impossiblePP,
  getArrow, getArrows,
  showExprRaw,
  mkEStr, mkExn,
  getAppM, getApp,
  TyVar, freeTyVars,
  getImplies,
  dropForallContext,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.ByteString(ByteString)
import Data.List
import Data.Maybe
import MicroHs.Builtin
import MicroHs.Ident
import Text.PrettyPrint.HughesPJLiteClass

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportItem] [EDef]
  deriving (Show)

data ExportItem
  = ExpModule IdentModule
  | ExpTypeSome Ident [Ident]
  | ExpValue Ident
  | ExpDefault Ident
  deriving (Show)

data EDef
  = Data LHS [Constr] [Deriving]
  | Newtype LHS Constr [Deriving]
  | Type LHS EType
  | Fcn Ident [Eqn]
  | PatBind EPat Expr
  | Sign [Ident] EType
  | KindSign Ident EKind
  | Import ImportSpec
  | ForImp CallConv (Maybe String) Ident EType
  | ForExp CallConv (Maybe String) Expr EType
  | Infix Fixity [Ident]
  | Class [EConstraint] LHS [FunDep] [EBind]  -- XXX will probable need initial forall with FD
  | Instance EConstraint [EBind] [EBind]      -- second set of bindings are the non-method bindings
  | Default (Maybe Ident) [EType]
  | Pattern LHS EPat (Maybe [Eqn])
  | StandDeriving DerStrategy Int EConstraint
  | DfltSign Ident EType                      -- only in class declarations
  -- Only used by interactive system to load a cached TCState to avoid import processing.
  -- We don't want to introduce a circular reference between Expr and TCMonad,
  -- because then we cannot bootstrap with Hugs.
  -- So instead we use dummy placeholder type and then use unsafeCoerce for the values.
  | SetTCState XTCState
  deriving (Show)

instance NFData EDef where
  rnf (Data a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (Newtype a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (Type a b) = rnf a `seq` rnf b
  rnf (Fcn a b) = rnf a `seq` rnf b
  rnf (PatBind a b) = rnf a `seq` rnf b
  rnf (Sign a b) = rnf a `seq` rnf b
  rnf (KindSign a b) = rnf a `seq` rnf b
  rnf (Import a) = rnf a
  rnf (ForImp a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (ForExp a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (Infix a b) = rnf a `seq` rnf b
  rnf (Class a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (Instance a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (Default a b) = rnf a `seq` rnf b
  rnf (Pattern a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (StandDeriving a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (DfltSign a b) = rnf a `seq` rnf b
  rnf (SetTCState a) = seq a ()

data XTCState
instance Eq XTCState where _ == _  =  True
instance Show XTCState where show _ = "<<TCState>>"

data ImpType = ImpNormal | ImpBoot
  deriving (Eq, Show)

instance NFData ImpType where rnf x = x `seq` ()

data CallConv = Cccall | Ccapi | Cjavascript
  deriving (Eq, Show)

instance NFData CallConv where rnf x = x `seq` ()

data ImportSpec = ImportSpec ImpType Bool Ident (Maybe Ident) (Maybe (Bool, [ImportItem]))  -- first Bool indicates 'qualified', second 'hiding'
  deriving (Show)

instance NFData ImportSpec where
  rnf (ImportSpec a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

data ImportItem
  = ImpTypeSome Ident [Ident]
  | ImpTypeAll Ident
  | ImpValue Ident
  deriving (Show)

instance NFData ImportItem where
  rnf (ImpTypeSome a b) = rnf a `seq` rnf b
  rnf (ImpTypeAll a) = rnf a
  rnf (ImpValue a) = rnf a

data Deriving = Deriving DerStrategy [(Int, EConstraint)] -- The Int is added by the type checker, it indicates how many arguments to keep
  deriving (Show)

instance NFData Deriving where
  rnf (Deriving a b) = rnf a `seq` rnf b

data DerStrategy
  = DerNone
  | DerStock
  | DerNewtype
  | DerAnyClass
  | DerVia EType
  deriving (Show)

instance NFData DerStrategy where
  rnf (DerVia a) = rnf a
  rnf _ = ()

-- Indication that we don't want any implicit deriving
doNotDerive :: [Deriving] -> Bool
doNotDerive = any p
 where p (Deriving DerNone []) = True
       p _ = False

data Expr
  = EVar Ident
  | EApp Expr Expr
  | EOper Expr [(Ident, Expr)]
  | ELam SLoc [Eqn]
  | ELit SLoc Lit
  | ECase Expr [ECaseArm]
  | ELet [EBind] Expr
  | ETuple [Expr]
  | EParen Expr
  | EListish Listish
  | EDo (Maybe Ident) [EStmt]
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | EMultiIf EAlts
  | ESign Expr EType
  | ENegApp Expr
  | EUpdate Expr [EField]
  | ESelect [Ident]
  | ETypeArg EType           -- @type
  -- only in patterns
  | EAt Ident EPat
  | EViewPat Expr EPat
  | ELazy Bool EPat           -- True indicates ~p, False indicates !p
  | EOr [EPat]
  -- only in types
  | EForall QForm [IdKind] EType
  -- only while type checking
  | EUVar Int
  | EQVar Expr EType             -- already resolved identifier
  -- only after type checking
  | ECon Con
  deriving (Show)

instance NFData Expr where
  rnf (EVar a) = rnf a
  rnf (EApp a b) = rnf a `seq` rnf b
  rnf (EOper a b) = rnf a `seq` rnf b
  rnf (ELam a b) = rnf a `seq` rnf b
  rnf (ELit a b) = rnf a `seq` rnf b
  rnf (ECase a b) = rnf a `seq` rnf b
  rnf (ELet a b) = rnf a `seq` rnf b
  rnf (ETuple a) = rnf a
  rnf (EParen a) = rnf a
  rnf (EListish a) = rnf a
  rnf (EDo a b) = rnf a `seq` rnf b
  rnf (ESectL a b) = rnf a `seq` rnf b
  rnf (ESectR a b) = rnf a `seq` rnf b
  rnf (EIf a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (EMultiIf a) = rnf a
  rnf (ESign a b) = rnf a `seq` rnf b
  rnf (ENegApp a) = rnf a
  rnf (EUpdate a b) = rnf a `seq` rnf b
  rnf (ESelect a) = rnf a
  rnf (ETypeArg a) = rnf a
  rnf (EAt a b) = rnf a `seq` rnf b
  rnf (EViewPat a b) = rnf a `seq` rnf b
  rnf (ELazy a b) = rnf a `seq` rnf b
  rnf (EOr a) = rnf a
  rnf (EForall a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (EUVar a) = rnf a
  rnf (EQVar a b) = rnf a `seq` rnf b
  rnf (ECon a) = rnf a

data QForm = QImpl | QExpl | QReqd
  deriving (Show)

instance NFData QForm where
  rnf q = seq q ()

data EField
  = EField [Ident] Expr     -- a.b = e
  | EFieldPun [Ident]       -- a.b
  | EFieldWild              -- ..
  deriving (Show)

instance NFData EField where
  rnf (EField a b) = rnf a `seq` rnf b
  rnf (EFieldPun a) = rnf a
  rnf EFieldWild = ()

unEField :: EField -> ([Ident], Expr)
unEField (EField is e) = (is, e)
unEField _ = impossible

type FunDep = ([Ident], [Ident])

eLam :: [EPat] -> Expr -> Expr
eLam = eLamWithSLoc noSLoc

eLamWithSLoc :: SLoc -> [EPat] -> Expr -> Expr
eLamWithSLoc loc ps e = ELam loc $ eEqns ps e

eEqns :: [EPat] -> Expr -> [Eqn]
eEqns ps e = [eEqn ps e]

eEqn :: [EPat] -> Expr -> Eqn
eEqn ps e = Eqn ps (oneAlt e)

oneAlt :: Expr -> EAlts
oneAlt e = EAlts [([], e)] []

type FieldName = Ident

data Con
  = ConData ConTyInfo Ident [FieldName]
  | ConNew Ident [FieldName]
  | ConSyn Ident Int (Expr, EType)
  deriving(Show)

instance NFData Con where
  rnf (ConData a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (ConNew a b) = rnf a `seq` rnf b
  rnf (ConSyn a b c) = rnf a `seq` rnf b `seq` rnf c

data Listish
  = LList [Expr]
  | LCompr Expr [EStmt]
  | LFrom Expr
  | LFromTo Expr Expr
  | LFromThen Expr Expr
  | LFromThenTo Expr Expr Expr
  deriving(Show)

instance NFData Listish where
  rnf (LList a) = rnf a
  rnf (LCompr a b) = rnf a `seq` rnf b
  rnf (LFrom a) = rnf a
  rnf (LFromTo a b) = rnf a `seq` rnf b
  rnf (LFromThen a b) = rnf a `seq` rnf b
  rnf (LFromThenTo a b c) = rnf a `seq` rnf b `seq` rnf c

conIdent :: HasCallStack =>
            Con -> Ident
conIdent (ConData _ i _) = i
conIdent (ConNew i _) = i
conIdent (ConSyn i _ _) = i

conArity :: Con -> Int
conArity (ConData cs i _) = fromMaybe (error "conArity") $ lookup i cs
conArity (ConNew _ _) = 1
conArity (ConSyn _ n _) = n

conFields :: Con -> [FieldName]
conFields (ConData _ _ fs) = fs
conFields (ConNew _ fs) = fs
conFields ConSyn{} = []

instance Eq Con where
  (==) (ConData _ i _)   (ConData _ j _)   = i == j
  (==) (ConNew    i _)   (ConNew    j _)   = i == j
  (==) (ConSyn    i _ _) (ConSyn    j _ _) = i == j
  (==) _                 _                 = False

data Lit
  = LInt Int
  | LInt64 Int64
  | LInteger Integer
  | LDouble Double
  | LFloat Float
  | LRat Rational
  | LChar Char
  | LStr String
  | LBStr ByteString        -- bytestring
  | LPrim String
  | LExn String             -- exception to raise
  | LForImp ImpEnt String CType
  | LCType CType            -- used for foreign export
  | LTick String
  deriving (Eq, Show)

instance NFData Lit where
  rnf (LInt a) = rnf a
  rnf (LInt64 a) = rnf a
  rnf (LInteger a) = rnf a
  rnf (LDouble a) = rnf a
  rnf (LFloat a) = rnf a
  rnf (LRat a) = rnf a
  rnf (LChar a) = rnf a
  rnf (LStr a) = rnf a
  rnf (LBStr a) = rnf a
  rnf (LPrim a) = rnf a
  rnf (LExn a) = rnf a
  rnf (LForImp a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf (LCType e) = rnf e
  rnf (LTick a) = rnf a

-- A type of a C FFI function
newtype CType = CType EType
  deriving (Show)

instance Eq CType where
  _ == _  =  True    -- Just ignore the CType

instance NFData CType where
  rnf (CType t) = rnf t

data ImpEnt
  = ImpStatic [String] ImpVal String   -- includes, type of value, C name/expr
  | ImpDynamic
  | ImpWrapper
  | ImpJS String
  deriving (Eq, Show)

instance NFData ImpEnt where
  rnf (ImpStatic a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf ImpDynamic = ()
  rnf ImpWrapper = ()
  rnf (ImpJS s) = rnf s

data ImpVal = IPtr | IValue | IFunc
  deriving (Eq, Show)

instance NFData ImpVal where
  rnf a = seq a ()

---------------

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind] | SRec [EStmt]
  deriving (Show)

instance NFData EStmt where
  rnf (SBind a b) = rnf a `seq` rnf b
  rnf (SThen a) = rnf a
  rnf (SLet a) = rnf a
  rnf (SRec a) = rnf a

type EBind = EDef   -- subset with Fcn, PatBind, Sign, and DfltSign

-- A single equation for a function
data Eqn = Eqn [EPat] EAlts
  deriving (Show)

instance NFData Eqn where
  rnf (Eqn a b) = rnf a `seq` rnf b

data EAlts = EAlts [EAlt] [EBind]
  deriving (Show)

instance NFData EAlts where
  rnf (EAlts a b) = rnf a `seq` rnf b

type EAlt = ([EStmt], Expr)

type ConTyInfo = [(Ident, Int)]    -- All constructors with their arities

type EPat = Expr

isPConApp :: EPat -> Bool
isPConApp (EVar i) = isConIdent i
isPConApp (EApp f _) = isPConApp f
isPConApp (EAt _ p) = isPConApp p
isPConApp _ = True

-- Variables bound in a pattern.
-- Could use difference lists, but it seems a little slower.
patVars :: HasCallStack => EPat -> [Ident]
patVars apat =
  case apat of
    EVar i -> add i []
    EApp p1 p2 -> patVars p1 ++ patVars p2
    EOper p1 ips -> patVars p1 ++ concatMap (\ (i, p2) -> i `add` patVars p2) ips
    ELit _ _ -> []
    ETuple ps -> concatMap patVars ps
    EParen p -> patVars p
    EListish (LList ps) -> concatMap patVars ps
    ESign p _ -> patVars p
    EAt i p -> i `add` patVars p
    EViewPat _ p -> patVars p
    ELazy _ p -> patVars p
    ECon _ -> []
    EUpdate _ fs -> concatMap field fs
    ENegApp _ -> []
    EOr ps -> concatMap patVars ps
    _ -> error $ "patVars " ++ showExpr apat
  where add i is | isConIdent i || isDummyIdent i = is
                 | otherwise = i : is
        field (EField _ p) = patVars p
        field (EFieldPun is) = [last is]
        field EFieldWild = impossible

type LHS = (Ident, [IdKind])

data Constr = Constr
  [IdKind] [EConstraint]          -- existentials: forall vs . ctx =>
  Ident                           -- constructor name
  Bool                            -- if the constructor is written in infix notation
  (Either [SType] [ConstrField])  -- types or named fields
  deriving(Show)

instance NFData Constr where
  rnf (Constr a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

type ConstrField = (Ident, SType)              -- record label and type
type SType = (Bool, EType)                     -- the Bool indicates strict

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

type EConstraint = EType

data IdKind = IdKind Ident EKind
  deriving (Show)

instance Pretty IdKind where
  pPrintPrec l p (IdKind i k) = maybeParens (p < 0) $ pPrintPrec l (-1) i <+> text "::" <+> pPrintPrec l (-1) k

instance NFData IdKind where
  rnf (IdKind a b) = rnf a `seq` rnf b

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType
type ESort = EType

sKind :: ESort
sKind = EVar (mkIdent "Primitives.Kind")

kType :: EKind
kType = EVar (mkIdent "Primitives.Type")

kConstraint :: EKind
kConstraint = EVar (mkIdent "Primitives.Constraint")

-- XXX Switch to TupleN like GHC?
tupleConstr :: SLoc -> Int -> Ident
tupleConstr loc 0 = mkIdentSLoc loc "()"
tupleConstr   _ 1 = error "tupleConstr"  -- XXX could use Solo
tupleConstr loc n = mkIdentSLoc loc (replicate (n - 1) ',')

-- Check if it is a tuple constructor
getTupleConstr :: Ident -> Maybe Int
getTupleConstr i =
  case unIdent i of
    "()" -> Just 0
    ',':xs -> Just (length xs + 2)  -- "," is 2-tuple
    _ -> Nothing

getExprTuple :: EType -> Maybe [EType]
getExprTuple = loop []
  where loop ts (EApp f a) = loop (a:ts) f
        loop ts (EVar i) | Just n <- getTupleConstr i, length ts == n = Just ts
        loop _ _ = Nothing

-- Create a tuple selector, component i (0 based) of n
mkTupleSel :: Int -> Int -> Expr
mkTupleSel i n = eLam [ETuple [ EVar $ if k == i then x else dummyIdent | k <- [0 .. n - 1] ]] (EVar x)
  where x = mkIdent "$x"

eAppI :: Ident -> Expr -> Expr
eAppI i a = EApp (EVar i) a

eApp2 :: Expr -> Expr -> Expr -> Expr
eApp2 a b c = EApp (EApp a b) c

eAppI2 :: Ident -> Expr -> Expr -> Expr
eAppI2 a b c = EApp (EApp (EVar a) b) c

eApp3 :: Expr -> Expr -> Expr -> Expr -> Expr
eApp3 a b c d = EApp (eApp2 a b c) d

eAppI3 :: Ident -> Expr -> Expr -> Expr -> Expr
eAppI3 a b c d = EApp (eAppI2 a b c) d

eApps :: Expr -> [Expr] -> Expr
eApps = foldl EApp

eLetB :: [EBind] -> Expr -> Expr
eLetB [] e = e
eLetB bs e = ELet bs e

lhsToType :: LHS -> EType
lhsToType (i, iks) = eApps (EVar i) $ map (EVar . idKindIdent) iks

---------------------------------

-- Get the location of a syntactic element
class HasLoc a where
  getSLoc :: a -> SLoc

instance HasLoc Ident where
  getSLoc = slocIdent

instance HasLoc EDef where
  getSLoc (Fcn i _) = getSLoc i
  getSLoc (PatBind p _) = getSLoc p
  getSLoc (Sign i _) = getSLoc i
  getSLoc (DfltSign i _) = getSLoc i
  getSLoc (Infix _ is) = getSLoc is
  getSLoc (Instance t _ _) = getSLoc t
  getSLoc (StandDeriving _ _ t) = getSLoc t
  getSLoc _ = error "HasLoc EDef: unimplemented"

-- Approximate location; only identifiers and literals carry a location
instance HasLoc Expr where
  getSLoc (EVar i) = getSLoc i
  getSLoc (EApp f a) = getSLoc f `orSLoc` getSLoc a
  getSLoc (EOper e _) = getSLoc e
  getSLoc (ELam l _) = l
  getSLoc (ELit l _) = l
  getSLoc (ECase e _) = getSLoc e
  getSLoc (ELet bs _) = getSLoc bs
  getSLoc (ETuple es) = getSLoc es
  getSLoc (EParen e) = getSLoc e
  getSLoc (EListish l) = getSLoc l
  getSLoc (EDo (Just i) _) = getSLoc i
  getSLoc (EDo _ ss) = getSLoc ss
  getSLoc (ESectL e _) = getSLoc e
  getSLoc (ESectR i _) = getSLoc i
  getSLoc (EIf e _ _) = getSLoc e
  getSLoc (EMultiIf e) = getSLoc e
  getSLoc (ESign e _) = getSLoc e
  getSLoc (ENegApp e) = getSLoc e
  getSLoc (EUpdate e _) = getSLoc e
  getSLoc (ESelect is) = getSLoc is
  getSLoc (ETypeArg t) = getSLoc t
  getSLoc (EAt i _) = getSLoc i
  getSLoc (EViewPat e _) = getSLoc e
  getSLoc (ELazy _ e) = getSLoc e
  getSLoc (EOr es) = getSLoc es
  getSLoc (EUVar _) = noSLoc -- error "getSLoc EUVar"
  getSLoc (EQVar e _) = getSLoc e
  getSLoc (ECon c) = getSLoc c
  getSLoc (EForall _ iks e) = getSLoc iks `orSLoc` getSLoc e

instance HasLoc a => HasLoc [a] where
  getSLoc [] = noSLoc  -- XXX shouldn't happen
  getSLoc (a:_) = getSLoc a

instance HasLoc IdKind where
  getSLoc (IdKind i _) = getSLoc i

instance HasLoc Con where
  getSLoc (ConData _ i _) = getSLoc i
  getSLoc (ConNew i _) = getSLoc i
  getSLoc (ConSyn i _ _) = getSLoc i

instance HasLoc Listish where
  getSLoc (LList es) = getSLoc es
  getSLoc (LCompr e _) = getSLoc e
  getSLoc (LFrom e) = getSLoc e
  getSLoc (LFromTo e _) = getSLoc e
  getSLoc (LFromThen e _) = getSLoc e
  getSLoc (LFromThenTo e _ _) = getSLoc e

instance HasLoc EStmt where
  getSLoc (SBind p e) = getSLoc p `orSLoc` getSLoc e
  getSLoc (SThen e) = getSLoc e
  getSLoc (SLet bs) = getSLoc bs
  getSLoc (SRec bs) = getSLoc bs

instance HasLoc Eqn where
  getSLoc (Eqn [] a) = getSLoc a
  getSLoc (Eqn (p:_) _) = getSLoc p

instance HasLoc EAlts where
  getSLoc (EAlts as _) = getSLoc as

instance HasLoc a => HasLoc (a, b) where
  getSLoc (a, _) = getSLoc a

---------------------------------

data Assoc = AssocLeft | AssocRight | AssocNone
  deriving (Eq, Show)

type Fixity = (Assoc, Int)

instance NFData Assoc where rnf x = x `seq` ()

---------------------------------

-- Enough to handle subsitution in (undesguared) types
subst :: [(Ident, Expr)] -> Expr -> Expr
subst [] = id
subst s =
  let
    sub ae =
      case ae of
        EVar i -> fromMaybe ae $ lookup i s
        EApp f a -> EApp (sub f) (sub a)
        ESign e t -> ESign (sub e) t
        EUVar _ -> ae
        EForall b iks t | null (vs `intersect` is) -> EForall b iks $ subst s' t
                        | otherwise ->
                          -- We need to alpha convert to avoid accidental capture
                          let used = freeTyVars [t]
                              new = allBinders \\ (is ++ vs ++ used)
                              iks'  = zipWith (\ (IdKind _ k) n -> IdKind n k) iks new
                              alpha = zipWith (\ (IdKind i _) n -> (i, EVar n)) iks new
                          in  subst s' $ EForall b iks' $ subst alpha t
          where is = map idKindIdent iks
                s' = [ x | x@(i, _) <- s, i `notElem` is ]
                vs = freeTyVars (map snd s')    -- these are free in s'

        ELit _ _ -> ae
        ETuple ts -> ETuple (map sub ts)
        EOper t1 its -> EOper (sub t1) (map (second sub) its)
        EListish (LList [t]) -> EListish (LList [sub t])
        EParen t -> EParen (sub t)
        _ -> error $ "subst unimplemented " ++ showExpr ae
  in sub

allBinders :: [Ident] -- a,b,...,z,a1,a2,...
allBinders = [ mkIdent [x] | x <- ['a' .. 'z'] ] ++
             [ mkIdent ('a' : show i) | i <- [1::Int ..]]

---------------------------------

-- XXX needs more?
eqEType :: EType -> EType -> Bool
eqEType = eqExpr

-- Very partial implementation of Expr equality.
-- It is only used to compare instances, so this suffices.
eqExpr :: HasCallStack =>
          Expr -> Expr -> Bool
eqExpr (EVar i) (EVar i') = i == i'
eqExpr (ELit _ l) (ELit _ l') = l == l'
eqExpr (EApp f a) (EApp f' a') = eqExpr f f' && eqExpr a a'
eqExpr (EUVar r) (EUVar r') = r == r'
eqExpr (EForall _ iks t) (EForall _ iks' t') = map idKindIdent iks == map idKindIdent iks' && eqExpr t t'
eqExpr _ _ = False -- XXX good enough for instances
--eqExpr e1 e2 = error $ "eqExpr: unimplemented " ++ showExpr e1 ++ " == " ++ showExpr e2

---------------------------------

type DList a = [a] -> [a]

composeMap :: forall a b . (a -> DList b) -> [a] -> DList b
composeMap _ [] = id
composeMap f (x:xs) = f x . composeMap f xs

allVarsBind :: EBind -> [Ident]
allVarsBind b = allVarsBind' b []

allVarsBind' :: EBind -> DList Ident
allVarsBind' abind =
  case abind of
    Fcn i eqns -> (i:) . composeMap allVarsEqn eqns
    PatBind p e -> allVarsPat p . allVarsExpr' e
    Sign is _ -> (is ++)
    DfltSign i _ -> (i:)
    Infix _ _ -> id
    _ -> impossible

allVarsEqns :: HasCallStack => [Eqn] -> [Ident]
allVarsEqns eqns = composeMap allVarsEqn eqns []

allVarsEqn :: HasCallStack => Eqn -> DList Ident
allVarsEqn eqn =
  case eqn of
    Eqn ps alts -> composeMap allVarsPat ps . allVarsAlts alts

allVarsAlts :: EAlts -> DList Ident
allVarsAlts (EAlts alts bs) = composeMap allVarsAlt alts . composeMap allVarsBind' bs

allVarsAlt :: EAlt -> DList Ident
allVarsAlt (ss, e) = composeMap allVarsStmt ss . allVarsExpr' e

allVarsPat :: HasCallStack => EPat -> DList Ident
allVarsPat = allVarsExpr'

allVarsExpr :: HasCallStack => Expr -> [Ident]
allVarsExpr e = allVarsExpr' e []

allVarsExpr' :: HasCallStack => Expr -> DList Ident
allVarsExpr' aexpr =
  case aexpr of
    EVar i -> (i:)
    EApp e1 e2 -> allVarsExpr' e1 . allVarsExpr' e2
    EOper e1 ies -> allVarsExpr' e1 . composeMap (\ (i,e2) -> (i :) . allVarsExpr' e2) ies
    ELam _ qs -> composeMap allVarsEqn qs
    ELit _ _ -> id
    ECase e as -> allVarsExpr' e . composeMap allVarsCaseArm as
    ELet bs e -> composeMap allVarsBind' bs . allVarsExpr' e
    ETuple es -> composeMap allVarsExpr' es
    EParen e -> allVarsExpr' e
    EListish (LList es) -> composeMap allVarsExpr' es
    EDo mi ss -> maybe id (:) mi . composeMap allVarsStmt ss
    ESectL e i -> (i :) . allVarsExpr' e
    ESectR i e -> (i :) . allVarsExpr' e
    EIf e1 e2 e3 -> allVarsExpr' e1 . allVarsExpr' e2 . allVarsExpr' e3
    EMultiIf e -> allVarsAlts e
    EListish l -> allVarsListish l
    ESign e _ -> allVarsExpr' e
    ENegApp e -> allVarsExpr' e
    EUpdate e ies -> allVarsExpr' e . composeMap field ies
    ESelect _ -> id
    ETypeArg _ -> id
    EAt i e -> (i :) . allVarsExpr' e
    EViewPat e p -> allVarsExpr' e . allVarsExpr' p
    ELazy _ p -> allVarsExpr' p
    EOr ps -> composeMap allVarsExpr' ps
    EUVar _ -> id
    EQVar e _ -> allVarsExpr' e
    ECon c -> (conIdent c :)
    EForall _ iks e -> (map (\ (IdKind i _) -> i) iks ++) . allVarsExpr' e
  where field (EField _ e) = allVarsExpr' e
        field (EFieldPun is) = (last is :)
        field EFieldWild = id  -- XXX This is wrong.  But this can happen when there is no type signature
                               -- and the dependency analysis tries to find all used variables.
                               -- The correct fix is to desugar EFieldWild early and have tcDefsValue
                               -- only use free variables.

allVarsListish :: Listish -> DList Ident
allVarsListish (LList es) = composeMap allVarsExpr' es
allVarsListish (LCompr e ss) = allVarsExpr' e . composeMap allVarsStmt ss
allVarsListish (LFrom e) = allVarsExpr' e
allVarsListish (LFromTo e1 e2) = allVarsExpr' e1 . allVarsExpr' e2
allVarsListish (LFromThen e1 e2) = allVarsExpr' e1 . allVarsExpr' e2
allVarsListish (LFromThenTo e1 e2 e3) = allVarsExpr' e1 . allVarsExpr' e2 . allVarsExpr' e3

allVarsCaseArm :: ECaseArm -> DList Ident
allVarsCaseArm (p, alts) = allVarsPat p . allVarsAlts alts

allVarsStmt :: EStmt -> DList Ident
allVarsStmt astmt =
  case astmt of
    SBind p e -> allVarsPat p . allVarsExpr' e
    SThen e -> allVarsExpr' e
    SLet bs -> composeMap allVarsBind' bs
    SRec ss -> composeMap allVarsStmt ss

-----------------------------

setSLocExpr :: SLoc -> Expr -> Expr
setSLocExpr l (EVar i) = EVar (setSLocIdent l i)
setSLocExpr l (ECon c) = ECon (setSLocCon l c)
setSLocExpr l (ELit _ k) = ELit l k
setSLocExpr l (EApp f a) = EApp (setSLocExpr l f) a
setSLocExpr l (EForall q vs e) = EForall q vs (setSLocExpr l e)
setSLocExpr _ e@(EUVar _) = e
setSLocExpr _ e = error $ "setSLocExpr: unimpl " ++ show e  -- what other cases do we need?

setSLocCon :: SLoc -> Con -> Con
setSLocCon l (ConData ti i fs) = ConData ti (setSLocIdent l i) fs
setSLocCon l (ConNew i fs) = ConNew (setSLocIdent l i) fs
setSLocCon l (ConSyn i n m) = ConSyn (setSLocIdent l i) n m

errorMessage :: forall a .
                HasCallStack =>
                SLoc -> String -> a
errorMessage loc msg = mhsError $ showSLoc loc ++ ": " ++ msg

----------------

instance Pretty EModule where
  pPrintPrec l _ (EModule nm es ds) = text "module" <+> pPrint0 l nm <> parens (ppCommaSep (map (pPrint0 l) es)) <+> text "where" $$ ppEDefs l ds

instance Pretty ExportItem where
  pPrintPrec l _ (ExpModule m) = text "module" <+> pPrint0 l m
  pPrintPrec l _ (ExpTypeSome i is) = pPrint0 l i <> parens (ppCommaSep (map (pPrint0 l) is))
  pPrintPrec l _ (ExpValue i) = pPrint0 l i
  pPrintPrec l _ (ExpDefault i) = text "default" <+> pPrint0 l i

instance Pretty Eqn where
  pPrintPrec l _ eqn = ppEqns l (text "_") (text "=") [eqn]

showExpr :: Expr -> String
showExpr = render . ppExpr

showExprRaw :: Expr -> String
showExprRaw = render . ppExprRaw

showEDefs :: [EDef] -> String
showEDefs = render . ppEDefs prettyNormal

showEBind :: EBind -> String
showEBind = render . ppEBind

showEBinds :: [EBind] -> String
showEBinds = render . vcat . map ppEBind

showEType :: EType -> String
showEType = render . ppEType

ppEBind :: EBind -> Doc
ppEBind = pPrint

instance Pretty ImportItem where
  pPrintPrec l _ ae =
   case ae of
    ImpTypeSome i [] -> pPrint0 l i
    ImpTypeSome i is -> pPrint0 l i <> parens (ppCommaSep $ map (pPrint0 l) is)
    ImpTypeAll i -> pPrint0 l i <> text "(..)"
    ImpValue i -> pPrint0 l i

ppCommaSep :: [Doc] -> Doc
ppCommaSep = hsep . punctuate (text ",")

instance Pretty EDef where
  pPrintPrec l _ def =
   case def of
    Data lhs [] ds -> text "data" <+> ppLHS l lhs <+> ppDerivings l ds
    Data lhs cs ds -> text "data" <+> ppLHS l lhs <+> text "=" <+> hsep (punctuate (text " |") (map (ppConstr l) cs)) <+> ppDerivings l ds
    Newtype lhs c ds -> text "newtype" <+> ppLHS l lhs <+> text "=" <+> ppConstr l c <+> ppDerivings l ds
    Type lhs t -> text "type" <+> ppLHS l lhs <+> text "=" <+> pPrint0 l t
    Fcn i eqns -> ppEqns l (pPrint0 l i) (text "=") eqns
    PatBind p e -> ppEPat l p <+> text "=" <+> pPrint0 l e
    Sign is t -> ppCommaSep (map (pPrint0 l) is) <+> text "::" <+> pPrint0 l t
    KindSign i t -> text "type" <+> pPrint0 l i <+> text "::" <+> pPrint0 l t
    Import (ImportSpec b q m mm mis) -> text "import" <+>
      (if b == ImpBoot then text "{-# SOURCE #-}" else empty) <+>
      (if q then text "qualified" else empty) <+> pPrint0 l m <> text (maybe "" ((" as " ++) . unIdent) mm) <>
      case mis of
        Nothing -> empty
        Just (h, is) -> text (if h then " hiding" else "") <> parens (ppCommaSep (map (pPrint0 l) is))
    ForImp cc ie i t -> text "foreign import" <+> text (drop 1 $ show cc) <+> maybe empty (text . show) ie <+> pPrint0 l i <+> text "::" <+> pPrint0 l t
    ForExp cc ie e t -> text "foreign export" <+> text (drop 1 $ show cc) <+> maybe empty (text . show) ie <+> pPrint0 l e   <+> text "::" <+> pPrint0 l t
    Infix (a, p) is -> text ("infix" ++ f a) <+> text (show p) <+> ppCommaSep (map (pPrint0 l) is)
      where f AssocLeft = "l"; f AssocRight = "r"; f AssocNone = ""
    Class sup lhs fds bs -> ppWhere l (text "class" <+> ppCtx l sup <+> ppLHS l lhs <+> ppFunDeps l fds) bs
    Instance ct bs bs' -> ppWhere l (ppWhere l (text "instance" <+> pPrint0 l ct) bs) bs'
    Default mc ts -> text "default" <+> maybe empty (pPrint0 l) mc <+> parens (ppCommaSep (map (pPrint0 l) ts))
    Pattern lhs@(i,_) p meqns -> text "pattern" <+> ppLHS l lhs <+> text "=" <+> pPrint0 l p <+> maybe empty (ppWhere l (text ";") . (:[]) . Fcn i) meqns
    StandDeriving _s _narg ct -> text "deriving instance" <+> pPrint0 l ct
    DfltSign i t -> text "default" <+> pPrint0 l i <+> text "::" <+> pPrint0 l t
    SetTCState _ -> text "SetTCState ..."

ppDerivings :: PrettyLevel -> [Deriving] -> Doc
ppDerivings l = sep . map (ppDeriving l)

ppDeriving :: PrettyLevel -> Deriving -> Doc
ppDeriving l (Deriving s ds) = text "deriving" <+>
  case s of
    DerNone -> empty
    DerStock -> text "stock"
    DerNewtype -> text "newtype"
    DerAnyClass -> text "anyclass"
    DerVia _ -> empty
  <+> parens (hsep $ punctuate (text ",") (map (pPrint0 l . snd) ds))
  <+>
  case s of
    DerVia t -> text "via" <+> pPrint0 l t
    _ -> empty

ppCtx :: PrettyLevel -> [EConstraint] -> Doc
ppCtx _ [] = empty
ppCtx l ts = pPrint0 l (ETuple ts) <+> text "=>"

ppFunDeps :: PrettyLevel -> [FunDep] -> Doc
ppFunDeps _ [] = empty
ppFunDeps l fds =
  text "|" <+> ppCommaSep (map (\ (is, os) -> hsep (map (pPrint0 l) is) <+> text "-" <+> hsep (map (pPrint0 l) os)) fds)

ppEqns :: PrettyLevel -> Doc -> Doc -> [Eqn] -> Doc
ppEqns l name sepr = vcat . map (\ (Eqn ps alts) -> sep [name <+> hsep (map (ppEPat l) ps), ppAlts l sepr alts])

ppConstr :: PrettyLevel -> Constr -> Doc
ppConstr l (Constr iks ct c _ cs) = ppForall l QImpl iks <+> ppCtx l ct <+> pPrint0 l c <+> ppCs cs
  where ppCs (Left  ts) = hsep (map (ppSType l) ts)
        ppCs (Right fs) = braces (hsep $ map f fs)
          where f (i, t) = pPrint0 l i <+> text "::" <+> ppSType l t <> text ","

ppSType :: PrettyLevel -> SType -> Doc
ppSType l (False, t) = pPrint0 l t
ppSType l (True, t) = text "!" <> pPrint0 l t

ppLHS :: PrettyLevel -> LHS -> Doc
ppLHS l (f, vs) = hsep (pPrint0 l f : map (ppIdKind l) vs)

ppIdKind :: PrettyLevel -> IdKind -> Doc
ppIdKind l (IdKind i (EVar d)) | isDummyIdent d = pPrint0 l i
ppIdKind l (IdKind i k) = parens $ pPrint0 l i <> text "::" <> pPrint0 l k

ppEDefs :: PrettyLevel -> [EDef] -> Doc
ppEDefs l ds = vcat (map pp ds)
  where pp d@(Sign _ _) = pPrint0 l d
        pp d@(Import _) = pPrint0 l d
        pp d            = pPrint0 l d $+$ text ""

ppAlts :: PrettyLevel -> Doc -> EAlts -> Doc
ppAlts l asep (EAlts alts bs) = ppWhere l (ppAltsL l asep alts) bs

ppWhere :: PrettyLevel -> Doc -> [EBind] -> Doc
ppWhere _ d [] = d
ppWhere l d bs = (d <+> text "where") $+$ nest 2 (vcat (map (pPrint0 l) bs))

ppAltsL :: PrettyLevel -> Doc -> [EAlt] -> Doc
ppAltsL l asep [([], e)] = text "" <+> asep <+> pPrint0 l e
ppAltsL l asep alts = vcat (map (ppAlt l asep) alts)

ppAlt :: PrettyLevel -> Doc -> EAlt -> Doc
ppAlt l asep (ss, e) = text " |" <+> ppCommaSep (map (pPrint0 l) ss) <+> asep <+> pPrint0 l e

ppExprRaw :: Expr -> Doc
ppExprRaw = pPrintPrec (PrettyLevel 1) 0

ppExpr :: Expr -> Doc
ppExpr = pPrint

instance Pretty Expr where
  pPrintPrec l = ppE
   where
    raw = l > PrettyLevel 0
    ppE :: PrettyPrec -> Expr -> Doc
    ppE prec ae =
      case ae of
        EVar i | raw       -> text si
               | oper      -> parens (text op)
               | otherwise -> text s
                 where op = unIdent (unQualIdent i)
                       si = unIdent i
                       s = if "inst$" `isInfixOf` si then si else op
                       oper = case op of
                                [c] -> isOperChar c
                                c:d:_ -> isOperChar c && isOperChar d
                                _ -> False
        EApp _ _ -> ppApp prec [] ae
        EOper e ies -> ppE prec (foldl (\ e1 (i, e2) -> EApp (EApp (EVar i) e1) e2) e ies)
        ELam _ qs -> parens $ text "\\" <> ppEqns l empty (text "->") qs
        ELit _ i -> text (showLit i)
        ECase e as -> maybeParens (prec > 0) $ text "case" <+> ppE 0 e <+> text "of" $$ nest 2 (vcat (map (ppCaseArm l) as))
        ELet bs e -> maybeParens (prec > 0) $ text "let" $$ nest 2 (vcat (map ppEBind bs)) $$ text "in" <+> ppE 0 e
        ETuple es -> parens $ hsep $ punctuate (text ",") (map (ppE 0) es)
        EParen e -> parens (ppE 0 e)
        EDo mn ss -> maybeParens (prec > 0) $ maybe (text "do") (\ n -> pPrint0 l n <> text ".do") mn $$ nest 2 (vcat (map (pPrint0 l) ss))
        ESectL e i -> parens $ ppE appPrec e <+> pPrint0 l i
        ESectR i e -> parens $ pPrint0 l i <+> ppE appPrec e
        EIf e1 e2 e3 -> maybeParens (prec > 0) $ sep [text "if" <+> ppE 0 e1, text "then" <+> ppE 0 e2, text "else" <+> ppE 0 e3]
        EMultiIf e -> maybeParens (prec > 0) $ text "if" <+> ppAlts l (text "->") e
        EListish lst -> ppListish l lst
        ESign e t -> maybeParens (prec > 0) $ ppE 1 e <+> text "::" <+> ppE 1 t
        ENegApp e -> text "-" <+> ppE 6 e
        EUpdate ee ies -> ppE 12 ee <> text "{" <+> hsep (punctuate (text ",") (map (ppField l) ies)) <+> text "}"
        ESelect is -> maybeParens (prec > appPrec) $ hcat $ map (\ i -> text "." <> pPrint0 l i) is
        ETypeArg t -> text "@" <> ppE appPrec t
        EAt i e -> pPrint0 l i <> text "@" <> ppE appPrec e
        EViewPat e p -> parens $ ppE appPrec e <+> text "->" <+> ppE appPrec p
        ELazy True p -> text "~" <> ppE appPrec p
        ELazy False p -> text "!" <> ppE appPrec p
        EOr ps -> parens $ hsep (punctuate (text ";") (map (ppE 0) ps))
        EUVar i -> text ("_a" ++ show i)
        EQVar e t -> parens $ ppE 0 e <> text ":::" <> ppE 0 t
        ECon c -> {-text "***" <>-} ppCon l c
        EForall q iks e -> maybeParens (prec > 0) $ ppForall l q iks <+> ppE 0 e

    ppApp :: PrettyPrec -> [Expr] -> Expr -> Doc
    ppApp prec as (EApp f a) = ppApp prec (a:as) f
    ppApp prec as f | raw = ppApply prec f as
    ppApp prec as (EVar i) | isOperChar cop, [a, b] <- as = maybeParens (prec > p) $ ppE pl a <+> text op <+> ppE pr b
                           | isOperChar cop, [a] <- as    = parens $ ppE appPrec a <+> text op
                           | cop == ',' && length op + 1 == length as
                                                          = ppE prec (ETuple as)
                           | op == "[]", length as == 1   = ppE prec (EListish (LList as))
      where op = unIdent (unQualIdent i)
            cop = head op
            (pl, p, pr) = lookupPrec op
    ppApp prec as f = ppApply prec f as
    ppApply prec f as = maybeParens (prec > appPrec) $ hsep (map (ppE (appPrec + 1)) (f:as))

-- We don't have the fixity tables, so just hardcode a few common operators.
lookupPrec :: String -> (Int, Int, Int)
lookupPrec s = fromMaybe (10, 9, 10) $ lookup s   -- put parens both left and right on unknown operators
  [ ("->", (1, 0, 0))
  , ("=>", (1, 0, 0))
  ]

ppField :: PrettyLevel -> EField -> Doc
ppField l (EField is e) = hcat (punctuate (text ".") (map (pPrint0 l) is)) <+> text "=" <+> ppExpr e
ppField l (EFieldPun is) = hcat (punctuate (text ".") (map (pPrint0 l) is))
ppField _ EFieldWild = text ".."

ppForall :: PrettyLevel -> QForm -> [IdKind] -> Doc
--ppForall [] = empty
ppForall l q iks = text "forall" <+> hsep (map (ppIdKind l) iks) <+> text qs
  where qs = case q of QReqd -> "->"; _ -> "."

ppListish :: PrettyLevel -> Listish -> Doc
ppListish l (LList es) = ppList (pPrint0 l) es
ppListish l (LCompr e ss) = brackets $ pPrint0 l e <+> text "|" <+> ppCommaSep (map (pPrint0 l) ss)
ppListish l (LFrom e1) = brackets $ pPrint0 l e1 <> text ".."
ppListish l (LFromTo e1 e2) = brackets $ pPrint0 l e1 <> text ".." <> pPrint0 l e2
ppListish l (LFromThen e1 e2) = brackets $ pPrint0 l e1 <> text "," <> pPrint0 l e2 <> text ".."
ppListish l (LFromThenTo e1 e2 e3) = brackets $ pPrint0 l e1 <> text "," <> pPrint0 l e2 <> text ".." <> pPrint0 l e3

ppCon :: PrettyLevel -> Con -> Doc
ppCon l (ConData _ s _) = pPrint0 l s
ppCon l (ConNew s _) = pPrint0 l s
ppCon l (ConSyn s _ _) = pPrint0 l s

-- Literals are tagged the way they appear in the combinator file:
--  #   Int
--  ##  Int64
--  %   Integer
--  &   Double
--  &&  Float
--  '   Char    (not in file)
--  "   String
--  ^   FFI function
--      primitive
showLit :: Lit -> String
showLit = render . pPrint

instance Pretty Lit where
  pPrintPrec _ _ lit =
   text $
   case lit of
    LInt i     -> '#' : show i
    LInt64 i   -> '#' : '#' : show i
    LInteger i -> '%' : show i
    LDouble d  -> '&' : show d
    LFloat d   -> '&' : '&' : show d
    LRat r     -> '%' : show r
    LChar c    -> show c
    LStr s     -> show s
    LBStr s    -> show s
    LPrim s    -> s
    LExn s     -> s
    LForImp ie s _-> '^' : if isPtr then '&':s else s
      where isPtr = case ie of ImpStatic _ IPtr _ -> True; _ -> False
    LCType (CType t) -> showEType t
    LTick s    -> '!' : s

instance Pretty EStmt where
  pPrintPrec l _ as =
    case as of
      SBind p e -> ppEPat l p <+> text "<-" <+> pPrint0 l e
      SThen e -> pPrint0 l e
      SLet bs -> text "let" $$ nest 2 (vcat (map (pPrint0 l) bs))
      SRec ss -> text "rec" $$ nest 2 (vcat (map (pPrint0 l) ss))

ppCaseArm :: PrettyLevel -> ECaseArm -> Doc
ppCaseArm l arm =
  case arm of
    (p, alts) -> ppEPat l p <> ppAlts l (text "->") alts

ppEPat :: PrettyLevel -> EPat -> Doc
ppEPat = pPrint0

ppEType :: EType -> Doc
ppEType = pPrint0 prettyNormal

ppList :: forall a . (a -> Doc) -> [a] -> Doc
ppList pp xs = brackets $ hsep $ punctuate (text ",") (map pp xs)

getBindVars :: HasCallStack => EBind -> [Ident]
getBindVars abind =
  case abind of
    Fcn i _  -> [i]
    PatBind p _  -> patVars p
    _ -> []

getBindsVars :: HasCallStack => [EBind] -> [Ident]
getBindsVars = concatMap getBindVars

getStmtBound :: EStmt -> [Ident]
getStmtBound (SBind p _) = patVars p
getStmtBound (SThen _) = []
getStmtBound (SLet bs) = concatMap getBindVars bs
getStmtBound (SRec ss) = concatMap getStmtBound ss

eForall :: [IdKind] -> EType -> EType
eForall = eForall' QExpl

eForall' :: QForm -> [IdKind] -> EType -> EType
eForall' _ [] t = t
eForall' b vs t = EForall b vs t

unForall :: EType -> ([IdKind], EType)
unForall (EForall _ xs t) = (xs, t)
unForall               t  = ([], t)

eDummy :: Expr
eDummy = EVar dummyIdent

impossible :: forall a .
              HasCallStack =>
              a
impossible = error "impossible"

impossibleShow :: forall a b .
                  (HasCallStack, Show a, HasLoc a) => a -> b
impossibleShow a = error $ "impossible: " ++ prettyShow (getSLoc a) ++ " " ++ show a

impossiblePP :: forall a b .
                  (HasCallStack, Pretty a, HasLoc a) => a -> b
impossiblePP a = error $ "impossible: " ++ prettyShow (getSLoc a) ++ " " ++ prettyShow a

-----------

-- Probably belongs somewhere else
getArrow :: EType -> Maybe (EType, EType)
getArrow (EApp (EApp (EVar n) a) b) =
  if isIdent "->" n || isIdent "Primitives.->" n then Just (a, b) else Nothing
getArrow _ = Nothing

getArrows :: EType -> ([EType], EType)
getArrows at =
  case getArrow at of
    Nothing -> ([], at)
    Just (t, r) -> first (t:) (getArrows r)

mkEStr :: SLoc -> String -> Expr
mkEStr loc str = ESign (ELit loc (LStr str)) $ EListish $ LList [EVar $ mkBuiltin loc "Char"]

-- Make a call to generate an exception with location info
mkExn :: SLoc -> String -> String -> Expr
mkExn loc msg exn =
  let str = mkEStr loc $ msg ++ ", at " ++ prettyShow loc
      fn  = ELit loc $ LExn $ "Control.Exception.Internal." ++ exn
  in  EApp fn str

getAppM :: HasCallStack => EType -> Maybe (Ident, [EType])
getAppM = loop []
  where loop as (EVar i) = Just (i, as)
        loop as (EApp f a) = loop (a:as) f
        loop as (EParen e) = loop as e
        loop as (EOper e1 [(op, e2)]) = loop as $ EApp (EApp (EVar op) e1) e2
        loop _ _ = Nothing

getApp :: HasCallStack => EType -> (Ident, [EType])
getApp t = fromMaybe (impossiblePP t) $ getAppM t

type TyVar = Ident

freeTyVars :: [EType] -> [TyVar]
-- Get the free TyVars from a type; no duplicates in result
freeTyVars = foldr (go []) []
  where
    go :: [TyVar] -- Ignore occurrences of bound type variables
       -> EType   -- Type to look at
       -> [TyVar] -- Accumulates result
       -> [TyVar]
    go bound (EVar tv) acc
      | tv `elem` bound = acc
      | tv `elem` acc = acc
      | isConIdent tv = acc
      | otherwise = tv : acc
    go bound (EForall _ tvs ty) acc = go (map idKindIdent tvs ++ bound) ty acc
    go bound (EApp fun arg) acc = go bound fun (go bound arg acc)
    go _bound (EUVar _) acc = acc
    go _bound (ECon _) acc = acc
    go _bound (ELit _ _) acc = acc
    go bound (EOper e ies) acc = go bound e (goList bound (map snd ies) acc)
    go bound (ESign e _) acc = go bound e acc
    go bound (EListish (LList [e])) acc = go bound e acc
    go bound (ETuple es) acc = goList bound es acc
    go bound (EParen e) acc = go bound e acc
    go bound (EQVar e _) acc = go bound e acc
    go _ x _ = error ("freeTyVars: " ++ showEType x) --  impossibleShow x
    goList bound es acc = foldr (go bound) acc es

getImplies :: EType -> Maybe (EConstraint, EType)
getImplies (EApp (EApp (EVar n) a) b) =
  if isIdent "=>" n || isIdent "Primitives.=>" n then Just (a, b) else Nothing
getImplies _ = Nothing

dropForallContext :: EType -> EType
dropForallContext (EForall _ _ t) = dropForallContext t
dropForallContext t | Just (_, t') <- getImplies t = dropForallContext t'
                    | otherwise = t
