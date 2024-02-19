module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportItem(..),
  ImportSpec(..),
  ImportItem(..),
  EDef(..), showEDefs,
  Expr(..), eLam, eEqn, eEqns, showExpr, eqExpr,
  Listish(..),
  Lit(..), showLit,
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
  pp, pshow,
  ) where
import Prelude hiding ((<>))
import Data.List
import Data.Maybe
import MicroHs.Ident
import Text.PrettyPrint.HughesPJClass
import GHC.Stack

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportItem] [EDef]
--DEBUG  deriving (Show)

data ExportItem
  = ExpModule IdentModule
  | ExpTypeCon Ident
  | ExpType Ident
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
  | ForImp String Ident EType
  | Infix Fixity [Ident]
  | Class [EConstraint] LHS [FunDep] [EBind]  -- XXX will probable need initial forall with FD
  | Instance EConstraint [EBind]  -- no deriving yet
  | Default [EType]
--DEBUG  deriving (Show)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident) (Maybe (Bool, [ImportItem]))  -- first Bool indicates 'qualified', second 'hiding'
--DEBUG  deriving (Show)

data ImportItem
  = ImpTypeCon Ident
  | ImpType Ident
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
  -- NOTE: (- e) is not a section, it's a negation.  So we reuse ESect to mean negation in this case.
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ESign Expr EType
  | ENegApp Expr
  | EUpdate Expr [EField]
  | ESelect [Ident]
  -- only in patterns
  | EAt Ident Expr
  | EViewPat Expr EPat
  -- Only while type checking
  | EUVar Int
  -- Constructors after type checking
  | ECon Con
  | EForall [IdKind] Expr -- only in types
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
  | LForImp String
  | LTick String
--DEBUG  deriving (Show)
  deriving (Eq)

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
--DEBUG  deriving (Show)

data EBind = BFcn Ident [Eqn] | BPat EPat Expr | BSign Ident EType
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

{-
instance Show IdKind where
  show (IdKind i k) = "(" ++ show i ++ "::" ++ show k ++ ")"
-}
instance Pretty IdKind where
  pPrintPrec l _p (IdKind i (EVar d)) | d == dummyIdent && l == prettyNormal = pp l i
  pPrintPrec l p (IdKind i k) = maybeParens (p > 0) $ pp l i <+> "::" <+> pPrintPrec l 1 k

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType
type ESort = EType

sKind :: ESort
sKind = EVar (Ident noSLoc "Primitives.Kind")

kType :: EKind
kType = EVar (Ident noSLoc "Primitives.Type")

kConstraint :: EKind
kConstraint = EVar (Ident noSLoc "Primitives.Constraint")

tupleConstr :: SLoc -> Int -> Ident
tupleConstr loc n = mkIdentSLoc loc (replicate (n - 1) ',')

-- Check if it is a suple constructor
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
  getSLoc (Ident l _) = l

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
setSLocExpr l (EVar i) = EVar (setSLocIdent l i)
setSLocExpr l (ECon c) = ECon (setSLocCon l c)
setSLocExpr l (ELit _ k) = ELit l k
setSLocExpr _ _ = error "setSLocExpr"  -- what other cases do we need?

setSLocCon :: SLoc -> Con -> Con
setSLocCon l (ConData ti i fs) = ConData ti (setSLocIdent l i) fs
setSLocCon l (ConNew i fs) = ConNew (setSLocIdent l i) fs

errorMessage :: forall a .
                HasCallStack =>
                SLoc -> String -> a
errorMessage loc msg = error $ showSLoc loc ++ ": " ++ msg

----------------

{-
instance Show EModule where
  show (EModule nm _ ds) = "module " ++ showIdent nm ++ "(...) where\n" ++ showEDefs ds

instance Show Expr where
  show = showExpr

instance Show Eqn where
  show eqn = render $ ppEqns (text "_") (text "=") [eqn]

instance Show EDef where
  show d = showEDefs [d]
-}

showExpr :: Expr -> String
showExpr = prettyShow

showEDefs :: [EDef] -> String
showEDefs = render . vcat . map pPrint

showEBind :: EBind -> String
showEBind = prettyShow

showEBinds :: [EBind] -> String
showEBinds = render . vcat . map pPrint

showEType :: EType -> String
showEType = prettyShow

-- These two are to satisfy the parser
instance Show EModule where show = pshow
instance Show Expr where show = pshow

instance Pretty EModule where
  pPrintPrec l _ (EModule mn es ds) =
    "module" <+> pp l mn <> parens (sep $ punctuate comma $ map (pp l) es) <+> "where" $$
    vcat (map (pp l) ds)
  
instance Pretty ExportItem where
  pPrintPrec l _ (ExpModule mn) = "module" <+> pp l mn
  pPrintPrec l _ (ExpTypeCon i) = pp l i <> "(..)"
  pPrintPrec l _ (ExpType    i) = pp l i
  pPrintPrec l _ (ExpValue   i) = pp l i

instance Pretty ImportItem where
  pPrintPrec l _ ae =
    case ae of
      ImpTypeCon i -> pp l i <> "(..)"
      ImpType i -> pp l i
      ImpValue i -> pp l i

instance Pretty EDef where
  pPrintPrec l _ def =
    case def of
      Data lhs [] ds -> "data" <+> ppLHS l lhs <+> ppDeriving l ds
      Data lhs cs ds -> "data" <+> ppLHS l lhs <+> "=" <+> hsep (punctuate (text " |") (map (pp l) cs)) <+> ppDeriving l ds
      Newtype lhs c ds -> text "newtype" <+> ppLHS l lhs <+> "=" <+> pp l c <+> ppDeriving l ds
      Type lhs t -> text "type" <+> ppLHS l lhs <+> text "=" <+> pp l t
      Fcn i eqns -> ppEqns l (pp l i) (text "=") eqns
      Sign is t -> hsep (punctuate comma (map (pp l) is)) <+> "::" <+> pp l t
      KindSign i t -> "type" <+> pp l i <+> "::" <+> pp l t
      Import (ImportSpec q m mm mis) -> "import" <+> (if q then "qualified" else empty) <+> pp l m <>
        maybe empty (\ i -> "as" <+> pp l i) mm <>
        case mis of
          Nothing -> empty
          Just (h, is) -> (if h then " hiding" else empty) <> parens (hsep $ punctuate comma (map (pp l) is))
      ForImp ie i t -> "foreign import ccall" <+> text (show ie) <+> pp l i <+> text "::" <+> pp l t
      Infix (a, p) is -> "infix" <> f a <+> text (show p) <+> hsep (punctuate comma (map (pp l) is))
        where f AssocLeft = "l"; f AssocRight = "r"; f AssocNone = ""
      Class sup lhs fds bs -> ppWhere l ("class" <+> ppCtx l sup <+> ppLHS l lhs <+> ppFunDeps l fds) bs
      Instance ct bs -> ppWhere l ("instance" <+> pp l ct) bs
      Default ts -> "default" <+> parens (hsep (punctuate comma (map (pp l) ts)))

ppDeriving :: PrettyLevel -> Deriving -> Doc
ppDeriving _ [] = empty
ppDeriving l ds = "deriving" <+> parens (hsep $ punctuate comma (map (pp l) ds))

ppCtx :: PrettyLevel -> [EConstraint] -> Doc
ppCtx _ [] = empty
ppCtx l ts = pp l (ETuple ts) <+> "=>"

ppFunDeps :: PrettyLevel -> [FunDep] -> Doc
ppFunDeps _ [] = empty
ppFunDeps l fds =
  "|" <+> hsep (punctuate comma (map (\ (is, os) -> hsep (map (pp l) is) <+> "->" <+> hsep (map (pp l) os)) fds))

ppEqns :: PrettyLevel -> Doc -> Doc -> [Eqn] -> Doc
ppEqns l name sepr = vcat . map (\ (Eqn ps alts) -> sep [name <+> hsep (map (pp l) ps), ppAlts l sepr alts])

instance Pretty Constr where
  pPrintPrec l _ (Constr iks ct c cs) = ppForall l iks <+> ppCtx l ct <+> pp l c <+> ppCs cs
    where ppCs (Left  ts) = hsep (map (ppSType l) ts)
          ppCs (Right fs) = braces (hsep $ map f fs)
            where f (i, t) = pp l i <+> text "::" <+> ppSType l t <> comma

ppSType :: PrettyLevel -> SType -> Doc
ppSType l (False, t) =        pp l t
ppSType l (True,  t) = "!" <> pp l t

ppLHS :: PrettyLevel -> LHS -> Doc
ppLHS l (f, vs) = hsep (pp l f : map (pp l) vs)

{-
ppEDefs :: PrettyLevel -> [EDef] -> Doc
ppEDefs l ds = vcat (map f ds)
  where f d@(Sign _ _) = pp l d
        f d@(Import _) = pp l d
        f d            = pp l d $+$ text ""
-}

ppAlts :: PrettyLevel -> Doc -> EAlts -> Doc
ppAlts l asep (EAlts alts bs) = ppWhere l (ppAltsL l asep alts) bs

ppWhere :: PrettyLevel -> Doc -> [EBind] -> Doc
ppWhere _ d [] = d
ppWhere l d bs = (d <+> "where") $+$ nest 2 (vcat (map (pp l) bs))

ppAltsL :: PrettyLevel -> Doc -> [EAlt] -> Doc
ppAltsL l asep [([], e)] = text "" <+> asep <+> pp l e
ppAltsL l asep alts = vcat (map (ppAlt l asep) alts)

ppAlt :: PrettyLevel -> Doc -> EAlt -> Doc
ppAlt l asep (ss, e) = " |" <+> hsep (punctuate comma (map (pp l) ss)) <+> asep <+> pp l e

instance Pretty Expr where
 pPrintPrec l _p ae =
  case ae of
    EVar i | isOperChar cop -> parens (text op)
           | otherwise      -> text s
             where op = unIdent (unQualIdent i)
                   s = if "inst$" `isPrefixOf` op then unIdent i else op
                   cop = head op
    EApp _ _ -> ppApp l [] ae
    EOper e ies -> pp l (foldl (\ e1 (i, e2) -> EApp (EApp (EVar i) e1) e2) e ies)
    ELam qs -> parens $ text "\\" <> ppEqns l empty "->" qs
    ELit _ i -> pp l i
    ECase e as -> text "case" <+> pp l e <+> text "of" $$ nest 2 (vcat (map (ppCaseArm l) as))
    ELet bs e -> text "let" $$ nest 2 (vcat (map (pp l) bs)) $$ text "in" <+> pp l e
    ETuple es -> parens $ hsep $ punctuate (text ",") (map (pp l) es)
    EDo mn ss -> maybe (text "do") (\ n -> pp l n <> text ".do") mn $$ nest 2 (vcat (map (pp l) ss))
    ESectL e i -> parens $ pp l e <+> pp l i
    ESectR i e -> parens $ pp l i <+> pp l e
    EIf e1 e2 e3 -> parens $ sep [text "if" <+> pp l e1, text "then" <+> pp l e2, text "else" <+> pp l e3]
    EListish lst -> pp l lst
    ESign e t -> parens $ pp l e <+> text "::" <+> pp l t
    ENegApp e -> text "-" <+> pp l e
    EUpdate ee ies -> pp l ee <> braces (hsep (punctuate comma (map (pp l) ies)))
    ESelect is -> parens $ hcat $ map (\ i -> text "." <> pp l i) is
    EAt i e -> pp l i <> text "@" <> pp l e
    EViewPat e pat -> parens $ pp l e <+> text "->" <+> pp l pat
    EUVar i -> text ("_a" ++ show i)
    ECon (ConData _ s _) -> pp l s
    ECon (ConNew s _) -> pp l s
    EForall iks e -> ppForall l iks <+> pp l e

ppApp :: PrettyLevel -> [Expr] -> Expr -> Doc
ppApp l as (EApp f a) = ppApp l (a:as) f
ppApp l as (EVar i) | isOperChar cop, [a, b] <- as = parens $ pp l a <+> text op <+> pp l b
                    | isOperChar cop, [a] <- as    = parens $ pp l a <+> text op
                    | cop == ','                   = pp l (ETuple as)
                    | op == "[]", length as == 1   = pp l (EListish (LList as))
  where op = unIdent (unQualIdent i)
        cop = head op
ppApp l as f = parens $ hsep (map (pp l) (f:as))

instance Pretty EField where
  pPrintPrec l _ (EField is e) = hcat (punctuate "." (map (pp l) is)) <+> "=" <+> pp l e
  pPrintPrec l _ (EFieldPun is) = hcat (punctuate "." (map (pp l) is))
  pPrintPrec _ _  EFieldWild = ".."

ppForall :: PrettyLevel -> [IdKind] -> Doc
ppForall l iks = "forall" <+> hsep (map (pp l) iks) <+> "."

instance Pretty Listish where
  pPrintPrec l _ (LList es) = ppList (pp l) es
  pPrintPrec l _ (LCompr e ss) = brackets $ pp l e <+> "|" <+> hsep (punctuate comma (map (pp l) ss))
  pPrintPrec l _ (LFrom e1) = brackets $ pp l e1 <> ".."
  pPrintPrec l _ (LFromTo e1 e2) = brackets $ pp l e1 <> ".." <> pp l e2
  pPrintPrec l _ (LFromThen e1 e2) = brackets $ pp l e1 <> comma <> pp l e2 <> ".."
  pPrintPrec l _ (LFromThenTo e1 e2 e3) = brackets $ pp l e1 <> comma <> pp l e2 <> ".." <> pp l e3

-- Literals are tagged the way they appear in the combinator file:
--  #   Int
--  %   Double
--  '   Char    (not in file)
--  "   String
--  ^   FFI function
--      primitive
instance Pretty Lit where
  pPrintPrec _ _ = text . showLit

showLit :: Lit -> String
showLit lit =
  case lit of
    LInt i     -> '#' : show i
    LInteger i -> '#' : '#' : show i
    LDouble d  -> '&' : show d
    LRat r     -> '%' : show r
    LChar c    -> show c
    LStr s     -> show s
    LUStr s    -> show s
    LPrim s    -> s
    LForImp s  -> '^' : s
    LTick s    -> '!' : s

instance Pretty EStmt where
  pPrintPrec l _ as =
    case as of
    SBind p e -> pp l p <+> "<-" <+> pp l e
    SThen e -> pp l e
    SLet bs -> "let" $$ nest 2 (vcat (map (pp l) bs))

instance Pretty EBind where
  pPrintPrec l _ ab =
    case ab of
      BFcn i eqns -> pp l (Fcn i eqns)
      BPat p e -> pp l p <+> "=" <+> pp l e
      BSign i t -> pp l i <+> "::" <+> pp l t

ppCaseArm :: PrettyLevel -> ECaseArm -> Doc
ppCaseArm l (p, alts) = pp l p <> ppAlts l "->" alts

ppList :: forall a . (a -> Doc) -> [a] -> Doc
ppList pr xs = brackets $ hsep $ punctuate comma (map pr xs)

getBindVars :: EBind -> [Ident]
getBindVars abind =
  case abind of
    BFcn i _  -> [i]
    BPat p _  -> patVars p
    BSign _ _ -> []

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
                  (Pretty a, HasLoc a) => a -> b
impossibleShow a = error $ "impossible: " ++ show (getSLoc a) ++ " " ++ pshow a

pp :: forall a . Pretty a => PrettyLevel -> a -> Doc
pp l = pPrintPrec l 0

pshow :: forall a . Pretty a => a -> String
pshow = prettyShow
