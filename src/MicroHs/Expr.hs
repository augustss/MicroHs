module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportItem(..),
  ImportSpec(..),
  ImportItem(..),
  ImpType(..),
  EDef(..), showEDefs,
  Expr(..), eLam, eEqn, eEqns, showExpr, eqExpr,
  Listish(..),
  Lit(..), showLit,
  CType(..),
  EBind(..), showEBind, showEBinds,
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
  tupleConstr, getTupleConstr,
  mkTupleSel,
  eApps,
  lhsToType,
  subst,
  allVarsExpr, allVarsBind, allVarsEqns,
  setSLocExpr,
  errorMessage,
  Assoc(..), Fixity,
  getBindsVars,
  HasLoc(..),
  eForall,
  eDummy,
  impossible, impossibleShow,
  getArrow, getArrows,
  showExprRaw,
  mkEStr, mkExn,
  getAppM,
  TyVar, freeTyVars,
  ) where
import Prelude hiding ((<>))
import Control.Arrow(first)
import Data.List
import Data.Maybe
import MicroHs.Ident
import Text.PrettyPrint.HughesPJLite
import GHC.Stack

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportItem] [EDef]
--DEBUG  deriving (Show)

data ExportItem
  = ExpModule IdentModule
  | ExpTypeSome Ident [Ident]
  | ExpTypeAll Ident
  | ExpValue Ident
--DEBUG  deriving (Show)

data EDef
  = Data LHS [Constr] Deriving
  | Newtype LHS Constr Deriving
  | Type LHS EType
  | Fcn Ident [Eqn]
  | Sign [Ident] EType
  | KindSign Ident EKind
  | Import ImportSpec
  | ForImp (Maybe String) Ident EType
  | Infix Fixity [Ident]
  | Class [EConstraint] LHS [FunDep] [EBind]  -- XXX will probable need initial forall with FD
  | Instance EConstraint [EBind]
  | Default [EType]
  | Pattern LHS EPat
--DEBUG  deriving (Show)

data ImpType = ImpNormal | ImpBoot
  deriving (Eq)

data ImportSpec = ImportSpec ImpType Bool Ident (Maybe Ident) (Maybe (Bool, [ImportItem]))  -- first Bool indicates 'qualified', second 'hiding'
--DEBUG  deriving (Show)

data ImportItem
  = ImpTypeSome Ident [Ident]
  | ImpTypeAll Ident
  | ImpValue Ident
--DEBUG  deriving (Show)

type Deriving = [EConstraint]

data Expr
  = EVar Ident
  | EApp Expr Expr
  | EOper Expr [(Ident, Expr)]
  | ELam [Eqn]
  | ELit SLoc Lit
  | ECase Expr [ECaseArm]
  | ELet [EBind] Expr
  | ETuple [Expr]
  | EListish Listish
  | EDo (Maybe Ident) [EStmt]
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ESign Expr EType
  | ENegApp Expr
  | EUpdate Expr [EField]
  | ESelect [Ident]
  -- only in patterns
  | EAt Ident EPat
  | EViewPat Expr EPat
  | ELazy Bool EPat           -- True indicates ~p, False indicates !p
  -- only in types
  | EForall [IdKind] EType
  -- only while type checking
  | EUVar Int
  -- only after type checking
  | ECon Con
--DEBUG  deriving (Show)

data EField
  = EField [Ident] Expr     -- a.b = e
  | EFieldPun [Ident]       -- a.b
  | EFieldWild              -- ..
--DEBUG  deriving (Show)

unEField :: EField -> ([Ident], Expr)
unEField (EField is e) = (is, e)
unEField _ = impossible

type FunDep = ([Ident], [Ident])

eLam :: [EPat] -> Expr -> Expr
eLam ps e = ELam $ eEqns ps e

eEqns :: [EPat] -> Expr -> [Eqn]
eEqns ps e = [eEqn ps e]

eEqn :: [EPat] -> Expr -> Eqn
eEqn ps e = Eqn ps (EAlts [([], e)] [])

type FieldName = Ident

data Con
  = ConData ConTyInfo Ident [FieldName]
  | ConNew Ident [FieldName]
--DEBUG  deriving(Show)

data Listish
  = LList [Expr]
  | LCompr Expr [EStmt]
  | LFrom Expr
  | LFromTo Expr Expr
  | LFromThen Expr Expr
  | LFromThenTo Expr Expr Expr
--DEBUG  deriving(Show)

conIdent :: HasCallStack =>
            Con -> Ident
conIdent (ConData _ i _) = i
conIdent (ConNew i _) = i

conArity :: Con -> Int
conArity (ConData cs i _) = fromMaybe (error "conArity") $ lookup i cs
conArity (ConNew _ _) = 1

conFields :: Con -> [FieldName]
conFields (ConData _ _ fs) = fs
conFields (ConNew _ fs) = fs

instance Eq Con where
  (==) (ConData _ i _) (ConData _ j _) = i == j
  (==) (ConNew    i _) (ConNew    j _) = i == j
  (==) _               _               = False

data Lit
  = LInt Int
  | LInteger Integer
  | LDouble Double
  | LRat Rational
  | LChar Char
  | LStr String
  | LUStr String            -- UTF-8 encoded string
  | LPrim String
  | LExn Ident              -- exception to raise
  | LForImp String CType
  | LTick String
--DEBUG  deriving (Show)
  deriving (Eq)

-- A type of a C FFI function
newtype CType = CType EType
instance Eq CType where
  _ == _  =  True    -- Just ignore the CType

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
--DEBUG  deriving (Show)

data EBind
  = BFcn Ident [Eqn]
  | BPat EPat Expr
  | BSign Ident EType
  | BDfltSign Ident EType     -- only in class declarations
--DEBUG  deriving (Show)

-- A single equation for a function
data Eqn = Eqn [EPat] EAlts
--DEBUG  deriving (Show)

data EAlts = EAlts [EAlt] [EBind]
--DEBUG  deriving (Show)

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
    EListish (LList ps) -> concatMap patVars ps
    ESign p _ -> patVars p
    EAt i p -> i `add` patVars p
    EViewPat _ p -> patVars p
    ELazy _ p -> patVars p
    ECon _ -> []
    EUpdate _ fs -> concatMap field fs
    ENegApp _ -> []
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
  (Either [SType] [ConstrField])  -- types or named fields
--DEBUG  deriving(Show)

type ConstrField = (Ident, SType)              -- record label and type
type SType = (Bool, EType)                     -- the Bool indicates strict

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

type EConstraint = EType

data IdKind = IdKind Ident EKind
--DEBUG  deriving (Show)

instance Show IdKind where
  show (IdKind i k) = "(" ++ show i ++ "::" ++ show k ++ ")"

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType
type ESort = EType

sKind :: ESort
sKind = EVar (mkQIdent "Primitives" "Kind")

kType :: EKind
kType = EVar (mkQIdent "Primitives" "Type")

kConstraint :: EKind
kConstraint = EVar (mkQIdent "Primitives" "Constraint")

tupleConstr :: SLoc -> Int -> Ident
tupleConstr loc n = mkIdentSLoc loc (replicate (n - 1) ',')

-- Check if it is a tuple constructor
getTupleConstr :: Ident -> Maybe Int
getTupleConstr i =
  case unIdent i of
    ',':xs -> Just (length xs + 2)  -- "," is 2-tuple
    _ -> Nothing

-- Create a tuple selector, component i (0 based) of n
mkTupleSel :: Int -> Int -> Expr
mkTupleSel i n = eLam [ETuple [ EVar $ if k == i then x else dummyIdent | k <- [0 .. n - 1] ]] (EVar x)
  where x = mkIdent "$x"

eApps :: Expr -> [Expr] -> Expr
eApps = foldl EApp

lhsToType :: LHS -> EType
lhsToType (i, iks) = eApps (EVar i) $ map (EVar . idKindIdent) iks

---------------------------------

-- Get the location of a syntactic element
class HasLoc a where
  getSLoc :: a -> SLoc

instance HasLoc Ident where
  getSLoc = slocIdent

-- Approximate location; only identifiers and literals carry a location
instance HasLoc Expr where
  getSLoc (EVar i) = getSLoc i
  getSLoc (EApp e _) = getSLoc e
  getSLoc (EOper e _) = getSLoc e
  getSLoc (ELam qs) = getSLoc qs
  getSLoc (ELit l _) = l
  getSLoc (ECase e _) = getSLoc e
  getSLoc (ELet bs _) = getSLoc bs
  getSLoc (ETuple es) = getSLoc es
  getSLoc (EListish l) = getSLoc l
  getSLoc (EDo (Just i) _) = getSLoc i
  getSLoc (EDo _ ss) = getSLoc ss
  getSLoc (ESectL e _) = getSLoc e
  getSLoc (ESectR i _) = getSLoc i
  getSLoc (EIf e _ _) = getSLoc e
  getSLoc (ESign e _) = getSLoc e
  getSLoc (ENegApp e) = getSLoc e
  getSLoc (EUpdate e _) = getSLoc e
  getSLoc (ESelect is) = getSLoc (head is)
  getSLoc (EAt i _) = getSLoc i
  getSLoc (EViewPat e _) = getSLoc e
  getSLoc (ELazy _ e) = getSLoc e
  getSLoc (EUVar _) = error "getSLoc EUVar"
  getSLoc (ECon c) = getSLoc c
  getSLoc (EForall [] e) = getSLoc e
  getSLoc (EForall iks _) = getSLoc iks

instance forall a . HasLoc a => HasLoc [a] where
  getSLoc [] = noSLoc  -- XXX shouldn't happen
  getSLoc (a:_) = getSLoc a

instance HasLoc IdKind where
  getSLoc (IdKind i _) = getSLoc i

instance HasLoc Con where
  getSLoc (ConData _ i _) = getSLoc i
  getSLoc (ConNew i _) = getSLoc i

instance HasLoc Listish where
  getSLoc (LList es) = getSLoc es
  getSLoc (LCompr e _) = getSLoc e
  getSLoc (LFrom e) = getSLoc e
  getSLoc (LFromTo e _) = getSLoc e
  getSLoc (LFromThen e _) = getSLoc e
  getSLoc (LFromThenTo e _ _) = getSLoc e

instance HasLoc EStmt where
  getSLoc (SBind p _) = getSLoc p
  getSLoc (SThen e) = getSLoc e
  getSLoc (SLet bs) = getSLoc bs

instance HasLoc EBind where
  getSLoc (BFcn i _) = getSLoc i
  getSLoc (BPat p _) = getSLoc p
  getSLoc (BSign i _) = getSLoc i
  getSLoc (BDfltSign i _) = getSLoc i

instance HasLoc Eqn where
  getSLoc (Eqn [] a) = getSLoc a
  getSLoc (Eqn (p:_) _) = getSLoc p

instance HasLoc EAlts where
  getSLoc (EAlts as _) = getSLoc as

instance HasLoc EAlt where
  getSLoc ([], e) = getSLoc e
  getSLoc (ss, _) = getSLoc ss

---------------------------------

data Assoc = AssocLeft | AssocRight | AssocNone
--DEBUG  deriving (Show)
  deriving (Eq)

type Fixity = (Assoc, Int)

---------------------------------

-- Enough to handle subsitution in types
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
        EForall iks t -> EForall iks $ subst [ x | x@(i, _) <- s, not (elem i is) ] t
          where is = map idKindIdent iks
        ELit _ _ -> ae
        _ -> error "subst unimplemented"
  in sub

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
    BFcn i eqns -> (i:) . composeMap allVarsEqn eqns
    BPat p e -> allVarsPat p . allVarsExpr' e
    BSign i _ -> (i:)
    BDfltSign i _ -> (i:)

allVarsEqns :: [Eqn] -> [Ident]
allVarsEqns eqns = composeMap allVarsEqn eqns []

allVarsEqn :: Eqn -> DList Ident
allVarsEqn eqn =
  case eqn of
    Eqn ps alts -> composeMap allVarsPat ps . allVarsAlts alts

allVarsAlts :: EAlts -> DList Ident
allVarsAlts (EAlts alts bs) = composeMap allVarsAlt alts . composeMap allVarsBind' bs

allVarsAlt :: EAlt -> DList Ident
allVarsAlt (ss, e) = composeMap allVarsStmt ss . allVarsExpr' e

allVarsPat :: EPat -> DList Ident
allVarsPat = allVarsExpr'

allVarsExpr :: Expr -> [Ident]
allVarsExpr e = allVarsExpr' e []

allVarsExpr' :: Expr -> DList Ident
allVarsExpr' aexpr =
  case aexpr of
    EVar i -> (i:)
    EApp e1 e2 -> allVarsExpr' e1 . allVarsExpr' e2
    EOper e1 ies -> allVarsExpr' e1 . composeMap (\ (i,e2) -> (i :) . allVarsExpr' e2) ies
    ELam qs -> composeMap allVarsEqn qs
    ELit _ _ -> id
    ECase e as -> allVarsExpr' e . composeMap allVarsCaseArm as
    ELet bs e -> composeMap allVarsBind' bs . allVarsExpr' e
    ETuple es -> composeMap allVarsExpr' es
    EListish (LList es) -> composeMap allVarsExpr' es
    EDo mi ss -> maybe id (:) mi . composeMap allVarsStmt ss
    ESectL e i -> (i :) . allVarsExpr' e
    ESectR i e -> (i :) . allVarsExpr' e
    EIf e1 e2 e3 -> allVarsExpr' e1 . allVarsExpr' e2 . allVarsExpr' e3
    EListish l -> allVarsListish l
    ESign e _ -> allVarsExpr' e
    ENegApp e -> allVarsExpr' e
    EUpdate e ies -> allVarsExpr' e . composeMap field ies
    ESelect _ -> id
    EAt i e -> (i :) . allVarsExpr' e
    EViewPat e p -> allVarsExpr' e . allVarsExpr' p
    ELazy _ p -> allVarsExpr' p
    EUVar _ -> id
    ECon c -> (conIdent c :)
    EForall iks e -> (map (\ (IdKind i _) -> i) iks ++) . allVarsExpr' e
  where field (EField _ e) = allVarsExpr' e
        field (EFieldPun is) = (last is :)
        field EFieldWild = impossible

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

-----------------------------

setSLocExpr :: SLoc -> Expr -> Expr
setSLocExpr l (EVar i) = EVar (setIdentSLoc l i)
setSLocExpr l (ECon c) = ECon (setSLocCon l c)
setSLocExpr l (ELit _ k) = ELit l k
setSLocExpr _ _ = error "setSLocExpr"  -- what other cases do we need?

setSLocCon :: SLoc -> Con -> Con
setSLocCon l (ConData ti i fs) = ConData ti (setIdentSLoc l i) fs
setSLocCon l (ConNew i fs) = ConNew (setIdentSLoc l i) fs

errorMessage :: forall a .
                HasCallStack =>
                SLoc -> String -> a
errorMessage loc msg = error $ showSLoc loc ++ ": " ++ msg

----------------

instance Show EModule where
  show (EModule nm _ ds) = "module " ++ showIdent nm ++ "(...) where\n" ++ showEDefs ds

instance Show Expr where
  show = showExpr

instance Show Eqn where
  show eqn = render $ ppEqns (text "_") (text "=") [eqn]

instance Show EDef where
  show d = showEDefs [d]

showExpr :: Expr -> String
showExpr = render . ppExpr

showExprRaw :: Expr -> String
showExprRaw = render . ppExprRaw

showEDefs :: [EDef] -> String
showEDefs = render . ppEDefs

showEBind :: EBind -> String
showEBind = render . ppEBind

showEBinds :: [EBind] -> String
showEBinds = render . vcat . map ppEBind

showEType :: EType -> String
showEType = render . ppEType

ppImportItem :: ImportItem -> Doc
ppImportItem ae =
  case ae of
    ImpTypeSome i [] -> ppIdent i
    ImpTypeSome i is -> ppIdent i <> parens (ppCommaSep $ map ppIdent is)
    ImpTypeAll i -> ppIdent i <> text "(..)"
    ImpValue i -> ppIdent i

ppCommaSep :: [Doc] -> Doc
ppCommaSep = hsep . punctuate (text ",")

ppEDef :: EDef -> Doc
ppEDef def =
  case def of
    Data lhs [] ds -> text "data" <+> ppLHS lhs <+> ppDeriving ds
    Data lhs cs ds -> text "data" <+> ppLHS lhs <+> text "=" <+> hsep (punctuate (text " |") (map ppConstr cs)) <+> ppDeriving ds
    Newtype lhs c ds -> text "newtype" <+> ppLHS lhs <+> text "=" <+> ppConstr c <+> ppDeriving ds
    Type lhs t -> text "type" <+> ppLHS lhs <+> text "=" <+> ppEType t
    Fcn i eqns -> ppEqns (ppIdent i) (text "=") eqns
    Sign is t -> hsep (punctuate (text ",") (map ppIdent is)) <+> text "::" <+> ppEType t
    KindSign i t -> text "type" <+> ppIdent i <+> text "::" <+> ppEKind t
    Import (ImportSpec b q m mm mis) -> text "import" <+>
      (if b == ImpBoot then text "{-# SOURCE #-}" else empty) <+>
      (if q then text "qualified" else empty) <+> ppIdent m <> text (maybe "" ((" as " ++) . unIdent) mm) <>
      case mis of
        Nothing -> empty
        Just (h, is) -> text (if h then " hiding" else "") <> parens (hsep $ punctuate (text ",") (map ppImportItem is))
    ForImp ie i t -> text "foreign import ccall" <+> maybe empty (text . show) ie <+> ppIdent i <+> text "::" <+> ppEType t
    Infix (a, p) is -> text ("infix" ++ f a) <+> text (show p) <+> hsep (punctuate (text ", ") (map ppIdent is))
      where f AssocLeft = "l"; f AssocRight = "r"; f AssocNone = ""
    Class sup lhs fds bs -> ppWhere (text "class" <+> ppCtx sup <+> ppLHS lhs <+> ppFunDeps fds) bs
    Instance ct bs -> ppWhere (text "instance" <+> ppEType ct) bs
    Default ts -> text "default" <+> parens (hsep (punctuate (text ", ") (map ppEType ts)))
    Pattern lhs p -> text "pattern" <+> ppLHS lhs <+> text "=" <+> ppExpr p

ppDeriving :: Deriving -> Doc
ppDeriving [] = empty
ppDeriving ds = text "deriving" <+> parens (hsep $ punctuate (text ",") (map ppExpr ds))

ppCtx :: [EConstraint] -> Doc
ppCtx [] = empty
ppCtx ts = ppEType (ETuple ts) <+> text "=>"

ppFunDeps :: [FunDep] -> Doc
ppFunDeps [] = empty
ppFunDeps fds =
  text "|" <+> hsep (punctuate (text ",") (map (\ (is, os) -> hsep (map ppIdent is) <+> text "-" <+> hsep (map ppIdent os)) fds))

ppEqns :: Doc -> Doc -> [Eqn] -> Doc
ppEqns name sepr = vcat . map (\ (Eqn ps alts) -> sep [name <+> hsep (map ppEPat ps), ppAlts sepr alts])

ppConstr :: Constr -> Doc
ppConstr (Constr iks ct c cs) = ppForall iks <+> ppCtx ct <+> ppIdent c <+> ppCs cs
  where ppCs (Left  ts) = hsep (map ppSType ts)
        ppCs (Right fs) = braces (hsep $ map f fs)
          where f (i, t) = ppIdent i <+> text "::" <+> ppSType t <> text ","

ppSType :: SType -> Doc
ppSType (False, t) = ppEType t
ppSType (True, t) = text "!" <> ppEType t

ppLHS :: LHS -> Doc
ppLHS (f, vs) = hsep (ppIdent f : map ppIdKind vs)

ppIdKind :: IdKind -> Doc
ppIdKind (IdKind i (EVar d)) | isDummyIdent d = ppIdent i
ppIdKind (IdKind i k) = parens $ ppIdent i <> text "::" <> ppEKind k

ppEDefs :: [EDef] -> Doc
ppEDefs ds = vcat (map pp ds)
  where pp d@(Sign _ _) = ppEDef d
        pp d@(Import _) = ppEDef d
        pp d            = ppEDef d $+$ text ""

ppAlts :: Doc -> EAlts -> Doc
ppAlts asep (EAlts alts bs) = ppWhere (ppAltsL asep alts) bs

ppWhere :: Doc -> [EBind] -> Doc
ppWhere d [] = d
ppWhere d bs = (d <+> text "where") $+$ nest 2 (vcat (map ppEBind bs))

ppAltsL :: Doc -> [EAlt] -> Doc
ppAltsL asep [([], e)] = text "" <+> asep <+> ppExpr e
ppAltsL asep alts = vcat (map (ppAlt asep) alts)

ppAlt :: Doc -> EAlt -> Doc
ppAlt asep (ss, e) = text " |" <+> hsep (punctuate (text ",") (map ppEStmt ss)) <+> asep <+> ppExpr e

ppExprRaw :: Expr -> Doc
ppExprRaw = ppExprR True

ppExpr :: Expr -> Doc
ppExpr = ppExprR False

ppExprR :: Bool -> Expr -> Doc
ppExprR raw = ppE
  where
    ppE :: Expr -> Doc
    ppE ae =
      case ae of
        EVar i | raw            -> text si
               | isOperChar cop -> parens (text op)
               | otherwise      -> text s
                 where op = unIdent (unQualIdent i)
                       si = unIdent i
                       s = if "inst$" `isInfixOf` si then si else op
                       cop = head op
        EApp _ _ -> ppApp [] ae
        EOper e ies -> ppE (foldl (\ e1 (i, e2) -> EApp (EApp (EVar i) e1) e2) e ies)
        ELam qs -> parens $ text "\\" <> ppEqns empty (text "->") qs
        ELit _ i -> text (showLit i)
        ECase e as -> text "case" <+> ppE e <+> text "of" $$ nest 2 (vcat (map ppCaseArm as))
        ELet bs e -> text "let" $$ nest 2 (vcat (map ppEBind bs)) $$ text "in" <+> ppE e
        ETuple es -> parens $ hsep $ punctuate (text ",") (map ppE es)
        EDo mn ss -> maybe (text "do") (\ n -> ppIdent n <> text ".do") mn $$ nest 2 (vcat (map ppEStmt ss))
        ESectL e i -> parens $ ppE e <+> ppIdent i
        ESectR i e -> parens $ ppIdent i <+> ppE e
        EIf e1 e2 e3 -> parens $ sep [text "if" <+> ppE e1, text "then" <+> ppE e2, text "else" <+> ppE e3]
        EListish l -> ppListish l
        ESign e t -> parens $ ppE e <+> text "::" <+> ppEType t
        ENegApp e -> text "-" <+> ppE e
        EUpdate ee ies -> ppE ee <> text "{" <+> hsep (punctuate (text ",") (map ppField ies)) <+> text "}"
        ESelect is -> parens $ hcat $ map (\ i -> text "." <> ppIdent i) is
        EAt i e -> ppIdent i <> text "@" <> ppE e
        EViewPat e p -> parens $ ppE e <+> text "->" <+> ppE p
        ELazy True p -> text "~" <> ppE p
        ELazy False p -> text "!" <> ppE p
        EUVar i -> text ("_a" ++ show i)
        ECon c -> ppCon c
        EForall iks e -> ppForall iks <+> ppEType e

    ppApp :: [Expr] -> Expr -> Doc
    ppApp as (EApp f a) = ppApp (a:as) f
    ppApp as f | raw = ppApply f as
    ppApp as (EVar i) | isOperChar cop, [a, b] <- as = parens $ ppE a <+> text op <+> ppExpr b
                      | isOperChar cop, [a] <- as    = parens $ ppE a <+> text op
                      | cop == ','                   = ppE (ETuple as)
                      | op == "[]", length as == 1   = ppE (EListish (LList as))
                        where op = unIdent (unQualIdent i)
                              cop = head op
    ppApp as f = ppApply f as
    ppApply f as = parens $ hsep (map ppE (f:as))

ppField :: EField -> Doc
ppField (EField is e) = hcat (punctuate (text ".") (map ppIdent is)) <+> text "=" <+> ppExpr e
ppField (EFieldPun is) = hcat (punctuate (text ".") (map ppIdent is))
ppField EFieldWild = text ".."

ppForall :: [IdKind] -> Doc
--ppForall [] = empty
ppForall iks = text "forall" <+> hsep (map ppIdKind iks) <+> text "."

ppListish :: Listish -> Doc
ppListish (LList es) = ppList ppExpr es
ppListish (LCompr e ss) = brackets $ ppExpr e <+> text "|" <+> hsep (punctuate (text ",") (map ppEStmt ss))
ppListish (LFrom e1) = brackets $ ppExpr e1 <> text ".."
ppListish (LFromTo e1 e2) = brackets $ ppExpr e1 <> text ".." <> ppExpr e2
ppListish (LFromThen e1 e2) = brackets $ ppExpr e1 <> text "," <> ppExpr e2 <> text ".."
ppListish (LFromThenTo e1 e2 e3) = brackets $ ppExpr e1 <> text "," <> ppExpr e2 <> text ".." <> ppExpr e3

ppCon :: Con -> Doc
ppCon (ConData _ s _) = ppIdent s
ppCon (ConNew s _) = ppIdent s

-- Literals are tagged the way they appear in the combinator file:
--  #   Int
--  %   Double
--  '   Char    (not in file)
--  "   String
--  ^   FFI function
--      primitive
showLit :: Lit -> String
showLit l =
  case l of
    LInt i     -> '#' : show i
    LInteger i -> '#' : '#' : show i
    LDouble d  -> '&' : show d
    LRat r     -> '%' : show r
    LChar c    -> show c
    LStr s     -> show s
    LUStr s    -> show s
    LPrim s    -> s
    LExn s     -> showIdent s
    LForImp s _-> '^' : last (words s)  -- XXX needs improvement
    LTick s    -> '!' : s

ppEStmt :: EStmt -> Doc
ppEStmt as =
  case as of
    SBind p e -> ppEPat p <+> text "<-" <+> ppExpr e
    SThen e -> ppExpr e
    SLet bs -> text "let" $$ nest 2 (vcat (map ppEBind bs))

ppEBind :: EBind -> Doc
ppEBind ab =
  case ab of
    BFcn i eqns -> ppEDef (Fcn i eqns)
    BPat p e -> ppEPat p <+> text "=" <+> ppExpr e
    BSign i t -> ppIdent i <+> text "::" <+> ppEType t
    BDfltSign i t -> text "default" <+> ppEBind (BSign i t)

ppCaseArm :: ECaseArm -> Doc
ppCaseArm arm =
  case arm of
    (p, alts) -> ppEPat p <> ppAlts (text "->") alts

ppEPat :: EPat -> Doc
ppEPat = ppExpr

ppEType :: EType -> Doc
ppEType = ppExpr

ppEKind :: EKind -> Doc
ppEKind = ppEType

ppList :: forall a . (a -> Doc) -> [a] -> Doc
ppList pp xs = brackets $ hsep $ punctuate (text ",") (map pp xs)

getBindVars :: EBind -> [Ident]
getBindVars abind =
  case abind of
    BFcn i _  -> [i]
    BPat p _  -> patVars p
    BSign _ _ -> []
    BDfltSign _ _ -> []

getBindsVars :: [EBind] -> [Ident]
getBindsVars = concatMap getBindVars

eForall :: [IdKind] -> EType -> EType
eForall [] t = t
eForall vs t = EForall vs t

eDummy :: Expr
eDummy = EVar dummyIdent

impossible :: forall a .
              HasCallStack =>
              a
impossible = error "impossible"

impossibleShow :: forall a b .
                  HasCallStack =>
                  (Show a, HasLoc a) => a -> b
impossibleShow a = error $ "impossible: " ++ show (getSLoc a) ++ " " ++ show a

-----------

-- Probably belongs somewhere else
getArrow :: EType -> Maybe (EType, EType)
getArrow (EApp (EApp (EVar n) a) b) =
  if n == mkIdent "->" || n == mkQIdent "Primitives" "->" then Just (a, b) else Nothing
getArrow _ = Nothing

getArrows :: EType -> ([EType], EType)
getArrows at = 
  case getArrow at of
    Nothing -> ([], at)
    Just (t, r) -> first (t:) (getArrows r)

mkEStr :: SLoc -> String -> Expr
mkEStr loc str = ESign (ELit loc (LStr str)) $ EListish $ LList [EVar $ mkIdentSLoc loc "Char"]

-- Make a call to generate an exception with location info
mkExn :: SLoc -> String -> String -> Expr
mkExn loc msg exn =
  let str = mkEStr loc $ msg ++ ", at " ++ show loc
      fn  = ELit loc $ LExn $ mkQIdentSLoc loc "Control.Exception.Internal" exn
  in  EApp fn str

getAppM :: HasCallStack => EType -> Maybe (Ident, [EType])
getAppM = loop []
  where loop as (EVar i) = Just (i, as)
        loop as (EApp f a) = loop (a:as) f
        loop _ _ = Nothing

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
      | elem tv bound = acc
      | elem tv acc = acc
      | isConIdent tv = acc
      | otherwise = tv : acc
    go bound (EForall tvs ty) acc = go (map idKindIdent tvs ++ bound) ty acc
    go bound (EApp fun arg) acc = go bound fun (go bound arg acc)
    go _bound (EUVar _) acc = acc
    go _bound (ECon _) acc = acc
    go _bound (ELit _ _) acc = acc
    go bound (EOper e ies) acc = go bound e (goList bound (map snd ies) acc)
    go bound (ESign e _) acc = go bound e acc
    go bound (EListish (LList [e])) acc = go bound e acc
    go bound (ETuple es) acc = goList bound es acc
    go _ x _ = error ("freeTyVars: " ++ show x) --  impossibleShow x
    goList bound es acc = foldr (go bound) acc es

