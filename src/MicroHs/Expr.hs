module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportItem(..),
  ImportSpec(..),
  ImportItem(..),
  EDef(..), showEDefs,
  Expr(..), eLam, eEqns, showExpr, eqExpr,
  Listish(..),
  Lit(..), showLit,
  EBind(..), showEBind, showEBinds,
  Eqn(..),
  EStmt(..),
  EAlts(..),
  EAlt,
  ECaseArm,
  FunDep,
  EType, showEType, eqEType,
  EConstraint,
  EPat, patVars, isPConApp,
  EKind, kType, kConstraint,
  IdKind(..), idKindIdent,
  LHS,
  Constr(..), ConstrField, SType,
  ConTyInfo,
  Con(..), conIdent, conArity,
  tupleConstr, getTupleConstr,
  mkTupleSel,
  subst,
  allVarsExpr, allVarsBind,
  setSLocExpr,
  errorMessage,
  Assoc(..), Fixity,
  getBindsVars,
  HasLoc(..),
  ) where
import Prelude hiding ((<>))
import Data.List
import Data.Maybe
import MicroHs.Ident
import Text.PrettyPrint.HughesPJ
import GHC.Stack

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportItem] [EDef]

data ExportItem
  = ExpModule IdentModule
  | ExpTypeCon Ident
  | ExpType Ident
  | ExpValue Ident
  deriving (Show)

data EDef
  = Data LHS [Constr]
  | Newtype LHS Constr
  | Type LHS EType
  | Fcn Ident [Eqn]
  | Sign Ident EType
  | KindSign Ident EKind
  | Import ImportSpec
  | ForImp String Ident EType
  | Infix Fixity [Ident]
  | Class [EConstraint] LHS [FunDep] [EBind]  -- XXX will probable need initial forall with FD
  | Instance EConstraint [EBind]  -- no deriving yet
  | Default [EType]
  deriving (Show)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident) (Maybe (Bool, [ImportItem]))  -- first Bool indicates 'qualified', second 'hiding'
  deriving (Show)

data ImportItem
  = ImpTypeCon Ident
  | ImpType Ident
  | ImpValue Ident
  deriving (Show)

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
  -- only in patterns
  | EAt Ident Expr
  | EViewPat Expr EPat
  -- Only while type checking
  | EUVar Int
  -- Constructors after type checking
  | ECon Con
  | EForall [IdKind] Expr -- only in types
  --deriving (Show, Eq)

type FunDep = ([Ident], [Ident])

eLam :: [EPat] -> Expr -> Expr
eLam ps e = ELam $ eEqns ps e

eEqns :: [EPat] -> Expr -> [Eqn]
eEqns ps e = [Eqn ps (EAlts [([], e)] [])]

data Con
  = ConData ConTyInfo Ident
  | ConNew Ident
  deriving(Show)

data Listish
  = LList [Expr]
  | LCompr Expr [EStmt]
  | LFrom Expr
  | LFromTo Expr Expr
  | LFromThen Expr Expr
  | LFromThenTo Expr Expr Expr
  --deriving(Show, Eq)

conIdent :: HasCallStack =>
            Con -> Ident
conIdent (ConData _ i) = i
conIdent (ConNew i) = i

conArity :: Con -> Int
conArity (ConData cs i) = fromMaybe (error "conArity") $ lookup i cs
conArity (ConNew _) = 1

instance Eq Con where
  (==) (ConData _ i) (ConData _ j) = i == j
  (==) (ConNew    i) (ConNew    j) = i == j
  (==) _             _             = False

data Lit
  = LInt Int
  | LInteger Integer
  | LDouble Double
  | LRat Rational
  | LChar Char
  | LStr String
  | LPrim String
  | LForImp String
  deriving (Show)

instance Eq Lit where
  (==) (LInt x)     (LInt  y) = x == y
  (==) (LInteger x) (LInteger  y) = x == y
  (==) (LDouble x)  (LDouble y) = x == y
  (==) (LRat x)     (LRat y) = x == y
  (==) (LChar x)    (LChar y) = x == y
  (==) (LStr  x)    (LStr  y) = x == y
  (==) (LPrim x)    (LPrim y) = x == y
  (==) (LForImp x)  (LForImp y) = x == y
  (==) _         _         = False

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
  deriving (Show)

data EBind = BFcn Ident [Eqn] | BPat EPat Expr | BSign Ident EType
  deriving (Show)

-- A single equation for a function
data Eqn = Eqn [EPat] EAlts

data EAlts = EAlts [EAlt] [EBind]
  deriving (Show)

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
patVars :: EPat -> [Ident]
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
    _ -> error $ "patVars " ++ showExpr apat
  where add i is | isConIdent i || isDummyIdent i = is
                 | otherwise = i : is

type LHS = (Ident, [IdKind])

data Constr = Constr
  [IdKind] [EConstraint]          -- existentials: forall vs . ctx =>
  Ident                           -- constructor name
  (Either [SType] [ConstrField])  -- types or named fields
  deriving(Show)

type ConstrField = (Ident, SType)              -- record label and type
type SType = (Bool, EType)                     -- the Bool indicates strict

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

type EConstraint = EType

data IdKind = IdKind Ident EKind
  --deriving (Show, Eq)

instance Show IdKind where
  show (IdKind i k) = "(" ++ show i ++ "::" ++ show k ++ ")"

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType

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
  getSLoc (EAt i _) = getSLoc i
  getSLoc (EViewPat e _) = getSLoc e
  getSLoc (EUVar _) = error "getSLoc EUVar"
  getSLoc (ECon c) = getSLoc c
  getSLoc (EForall [] e) = getSLoc e
  getSLoc (EForall iks _) = getSLoc iks

instance forall a . HasLoc a => HasLoc [a] where
  getSLoc [] = error "getSLoc []"
  getSLoc (a:_) = getSLoc a

instance HasLoc IdKind where
  getSLoc (IdKind i _) = getSLoc i

instance HasLoc Con where
  getSLoc (ConData _ i) = getSLoc i
  getSLoc (ConNew i) = getSLoc i

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
  deriving (Show)

instance Eq Assoc where
  AssocLeft  == AssocLeft  = True
  AssocRight == AssocRight = True
  AssocNone  == AssocNone  = True
  _          == _          = False

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
eqExpr (EVar _) (EApp _ _) = False
eqExpr (EApp f a) (EApp f' a') = eqExpr f f' && eqExpr a a'
eqExpr (EApp _ _) (EVar _) = False
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
    EAt i e -> (i :) . allVarsExpr' e
    EViewPat e p -> allVarsExpr' e . allVarsExpr' p
    EUVar _ -> id
    ECon c -> (conIdent c :)
    EForall iks e -> (map (\ (IdKind i _) -> i) iks ++) . allVarsExpr' e

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
setSLocCon l (ConData ti i) = ConData ti (setSLocIdent l i)
setSLocCon l (ConNew i) = ConNew (setSLocIdent l i)

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

showExpr :: Expr -> String
showExpr = render . ppExpr

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
    ImpTypeCon i -> ppIdent i <> text "(..)"
    ImpType i -> ppIdent i
    ImpValue i -> ppIdent i

ppEDef :: EDef -> Doc
ppEDef def =
  case def of
    Data lhs [] -> text "data" <+> ppLHS lhs
    Data lhs cs -> text "data" <+> ppLHS lhs <+> text "=" <+> hsep (punctuate (text " |") (map ppConstr cs))
    Newtype lhs c -> text "newtype" <+> ppLHS lhs <+> text "=" <+> ppConstr c
    Type lhs t -> text "type" <+> ppLHS lhs <+> text "=" <+> ppEType t
    Fcn i eqns -> ppEqns (ppIdent i) (text "=") eqns
    Sign i t -> ppIdent i <+> text "::" <+> ppEType t
    KindSign i t -> text "type" <+> ppIdent i <+> text "::" <+> ppEKind t
    Import (ImportSpec q m mm mis) -> text "import" <+> (if q then text "qualified" else empty) <+> ppIdent m <> text (maybe "" ((" as " ++) . unIdent) mm) <>
      case mis of
        Nothing -> empty
        Just (h, is) -> text (if h then " hiding" else "") <> parens (hsep $ punctuate (text ", ") (map ppImportItem is))
    ForImp ie i t -> text ("foreign import ccall " ++ show ie) <+> ppIdent i <+> text "::" <+> ppEType t
    Infix (a, p) is -> text ("infix" ++ f a) <+> text (show p) <+> hsep (punctuate (text ", ") (map ppIdent is))
      where f AssocLeft = "l"; f AssocRight = "r"; f AssocNone = ""
    Class sup lhs fds bs -> ppWhere (text "class" <+> ppCtx sup <+> ppLHS lhs <+> ppFunDeps fds) bs
    Instance ct bs -> ppWhere (text "instance" <+> ppEType ct) bs
    Default ts -> text "default" <+> parens (hsep (punctuate (text ", ") (map ppEType ts)))

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

ppExpr :: Expr -> Doc
ppExpr ae =
  case ae of
    EVar i | isOperChar cop -> parens (text op)
           | otherwise      -> text s
             where op = unIdent (unQualIdent i)
                   s = if "inst$" `isPrefixOf` op then unIdent i else op
                   cop = head op
    EApp _ _ -> ppApp [] ae
    EOper e ies -> ppExpr (foldl (\ e1 (i, e2) -> EApp (EApp (EVar i) e1) e2) e ies)
    ELam qs -> parens $ text "\\" <> ppEqns empty (text "->") qs
    ELit _ i -> text (showLit i)
    ECase e as -> text "case" <+> ppExpr e <+> text "of" $$ nest 2 (vcat (map ppCaseArm as))
    ELet bs e -> text "let" $$ nest 2 (vcat (map ppEBind bs)) $$ text "in" <+> ppExpr e
    ETuple es -> parens $ hsep $ punctuate (text ",") (map ppExpr es)
    EListish (LList es) -> ppList ppExpr es
    EDo mn ss -> maybe (text "do") (\ n -> ppIdent n <> text ".do") mn $$ nest 2 (vcat (map ppEStmt ss))
    ESectL e i -> parens $ ppExpr e <+> ppIdent i
    ESectR i e -> parens $ ppIdent i <+> ppExpr e
    EIf e1 e2 e3 -> parens $ sep [text "if" <+> ppExpr e1, text "then" <+> ppExpr e2, text "else" <+> ppExpr e3]
    EListish l -> ppListish l
    ESign e t -> ppExpr e <+> text "::" <+> ppEType t
    ENegApp e -> text "-" <+> ppExpr e
    EAt i e -> ppIdent i <> text "@" <> ppExpr e
    EViewPat e p -> parens $ ppExpr e <+> text "->" <+> ppExpr p
    EUVar i -> text ("a" ++ show i)
    ECon c -> ppCon c
    EForall iks e -> ppForall iks <+> ppEType e
--  where
ppApp :: [Expr] -> Expr -> Doc
ppApp as (EApp f a) = ppApp (a:as) f
ppApp as (EVar i) | isOperChar cop, [a, b] <- as = parens $ ppExpr a <+> text op <+> ppExpr b
                  | isOperChar cop, [a] <- as    = parens $ ppExpr a <+> text op
                  | cop == ','                   = ppExpr (ETuple as)
                  | op == "[]", length as == 1   = ppExpr (EListish (LList as))
                    where op = unIdent (unQualIdent i)
                          cop = head op
ppApp as f = parens $ hsep (map ppExpr (f:as))

ppForall :: [IdKind] -> Doc
--ppForall [] = empty
ppForall iks = text "forall" <+> hsep (map ppIdKind iks) <+> text "."

ppListish :: Listish -> Doc
ppListish _ = text "<<Listish>>"

ppCon :: Con -> Doc
ppCon (ConData _ s) = ppIdent s
ppCon (ConNew s) = ppIdent s

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
    LPrim s    -> s
    LForImp s  -> '^' : s

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

getBindsVars :: [EBind] -> [Ident]
getBindsVars = concatMap getBindVars
