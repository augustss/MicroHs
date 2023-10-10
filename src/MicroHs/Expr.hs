module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportItem(..),
  ImportSpec(..),
  ImportItem(..),
  EDef(..), showEDefs,
  Expr(..), showExpr,
  Listish(..),
  Lit(..), showLit, eqLit,
  EBind(..), showEBind,
  Eqn(..),
  EStmt(..),
  EAlts(..),
  EAlt,
  ECaseArm,
  EType, showEType,
  ETypeScheme,
  EPat, patVars, isPVar, isPConApp,
  EKind, kType,
  IdKind(..), idKindIdent,
  LHS,
  Constr(..),
  ConTyInfo,
  Con(..), conIdent, conArity, eqCon, getSLocCon,
  tupleConstr, untupleConstr, isTupleConstr,
  subst,
  allVarsExpr, allVarsBind,
  getSLocExpr, setSLocExpr,
  errorMessage,
  Assoc(..), eqAssoc, Fixity
  ) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
import Data.List
import Data.Maybe
import MicroHs.Ident
import qualified Data.Double as D
--Ximport Compat
--Ximport GHC.Stack
--Ximport Control.DeepSeq
--Yimport Primitives(NFData(..))

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportItem] [EDef]
  --Xderiving (Show, Eq)

data ExportItem
  = ExpModule IdentModule
  | ExpTypeCon Ident
  | ExpType Ident
  | ExpValue Ident
  --Xderiving (Show, Eq)

data EDef
  = Data LHS [Constr]
  | Newtype LHS Ident EType
  | Type LHS EType
  | Fcn Ident [Eqn]
  | Sign Ident EType
  | Import ImportSpec
  | ForImp String Ident EType
  | Infix Fixity [Ident]
  --Xderiving (Show, Eq)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident) (Maybe (Bool, [ImportItem]))  -- first Bool indicates 'qualified', second 'hiding'
  --Xderiving (Show, Eq)

data ImportItem
  = ImpTypeCon Ident
  | ImpType Ident
  | ImpValue Ident
  --Xderiving (Show, Eq)

data Expr
  = EVar Ident
  | EApp Expr Expr
  | EOper Expr [(Ident, Expr)]
  | ELam [EPat] Expr
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
  | EAt Ident Expr  -- only in patterns
  -- Only while type checking
  | EUVar Int
  -- Constructors after type checking
  | ECon Con
  | EForall [IdKind] Expr -- only in types
  --Xderiving (Show, Eq)

data Con
  = ConData ConTyInfo Ident
  | ConNew Ident
  | ConLit Lit
--  | ConTup Int
  --Xderiving(Show, Eq)

data Listish
  = LList [Expr]
  | LCompr Expr [EStmt]
  | LFrom Expr
  | LFromTo Expr Expr
  | LFromThen Expr Expr
  | LFromThenTo Expr Expr Expr
  --Xderiving(Show, Eq)

conIdent :: --XHasCallStack =>
            Con -> Ident
conIdent (ConData _ i) = i
conIdent (ConNew i) = i
conIdent _ = error "conIdent"

conArity :: Con -> Int
conArity (ConData cs i) = fromMaybe (error "conArity") $ lookupBy eqIdent i cs
conArity (ConNew _) = 1
conArity (ConLit _) = 0

eqCon :: Con -> Con -> Bool
eqCon (ConData _ i) (ConData _ j) = eqIdent i j
eqCon (ConNew    i) (ConNew    j) = eqIdent i j
eqCon (ConLit    l) (ConLit    k) = eqLit   l k
eqCon _             _             = False

data Lit
  = LInt Int
  | LDouble D.Double
  | LChar Char
  | LStr String
  | LPrim String
  | LForImp String
  --Xderiving (Show, Eq)
--Winstance NFData Lit where rnf (LInt i) = rnf i; rnf (LDouble d) = rnf d; rnf (LChar c) = rnf c; rnf (LStr s) = rnf s; rnf (LPrim s) = rnf s; rnf (LForImp s) = rnf s

eqLit :: Lit -> Lit -> Bool
eqLit (LInt x)  (LInt  y) = x == y
eqLit (LChar x) (LChar y) = eqChar x y
eqLit (LStr  x) (LStr  y) = eqString x y
eqLit (LPrim x) (LPrim y) = eqString x y
eqLit (LForImp x) (LForImp y) = eqString x y
eqLit _         _         = False

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
  --Xderiving (Show, Eq)

data EBind = BFcn Ident [Eqn] | BPat EPat Expr | BSign Ident EType
  --Xderiving (Show, Eq)

-- A single equation for a function
data Eqn = Eqn [EPat] EAlts
  --Xderiving (Show, Eq)

data EAlts = EAlts [EAlt] [EBind]
  --Xderiving (Show, Eq)

type EAlt = ([EStmt], Expr)

type ConTyInfo = [(Ident, Int)]    -- All constructors with their arities

type EPat = Expr

isPVar :: EPat -> Bool
isPVar (EVar i) = not (isConIdent i)
isPVar _ = False    

isPConApp :: EPat -> Bool
isPConApp (EVar i) = isConIdent i
isPConApp (EApp f _) = isPConApp f
isPConApp _ = True

patVars :: EPat -> [Ident]
patVars = filter (not . isConIdent) . allVarsExpr

type LHS = (Ident, [IdKind])

data Constr = Constr Ident [EType]
  --Xderiving(Show, Eq)

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

-- A type starting with an EForall
type ETypeScheme = EType

data IdKind = IdKind Ident EKind
  --Xderiving (Show, Eq)

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType

kType :: EKind
kType = EVar (Ident noSLoc "Primitives.Type")

tupleConstr :: SLoc -> Int -> Ident
tupleConstr loc n = mkIdentSLoc loc (replicate (n - 1) ',')

untupleConstr :: Ident -> Int
untupleConstr i = length (unIdent i) + 1

isTupleConstr :: Int -> Ident -> Bool
isTupleConstr n i = eqChar (head (unIdent i)) ',' && untupleConstr i == n

---------------------------------

data Assoc = AssocLeft | AssocRight | AssocNone
  --Xderiving (Eq, Show)

eqAssoc :: Assoc -> Assoc -> Bool
eqAssoc AssocLeft AssocLeft = True
eqAssoc AssocRight AssocRight = True
eqAssoc AssocNone AssocNone = True
eqAssoc _ _ = False

type Fixity = (Assoc, Int)

---------------------------------

-- Enough to handle subsitution in types
subst :: [(Ident, Expr)] -> Expr -> Expr
subst s =
  let
    sub ae =
      case ae of
        EVar i -> fromMaybe ae $ lookupBy eqIdent i s
        EApp f a -> EApp (sub f) (sub a)
        ESign e t -> ESign (sub e) t
        EUVar _ -> ae
        _ -> error "subst unimplemented"
  in sub

---------------------------------

allVarsBind :: EBind -> [Ident]
allVarsBind abind =
  case abind of
    BFcn i eqns -> i : concatMap allVarsEqn eqns
    BPat p e -> allVarsPat p ++ allVarsExpr e
    BSign i _ -> [i]

allVarsEqn :: Eqn -> [Ident]
allVarsEqn eqn =
  case eqn of
    Eqn ps alts -> concatMap allVarsPat ps ++ allVarsAlts alts

allVarsAlts :: EAlts -> [Ident]
allVarsAlts (EAlts alts bs) = concatMap allVarsAlt alts ++ concatMap allVarsBind bs

allVarsAlt :: EAlt -> [Ident]
allVarsAlt (ss, e) = concatMap allVarsStmt ss ++ allVarsExpr e

allVarsPat :: EPat -> [Ident]
allVarsPat = allVarsExpr

allVarsExpr :: Expr -> [Ident]
allVarsExpr aexpr =
  case aexpr of
    EVar i -> [i]
    EApp e1 e2 -> allVarsExpr e1 ++ allVarsExpr e2
    EOper e1 ies -> allVarsExpr e1 ++ concatMap (\ (i,e2) -> i : allVarsExpr e2) ies
    ELam ps e -> concatMap allVarsPat ps ++ allVarsExpr e
    ELit _ _ -> []
    ECase e as -> allVarsExpr e ++ concatMap allVarsCaseArm as
    ELet bs e -> concatMap allVarsBind bs ++ allVarsExpr e
    ETuple es -> concatMap allVarsExpr es
    EListish (LList es) -> concatMap allVarsExpr es
    EDo mi ss -> maybe [] (:[]) mi ++ concatMap allVarsStmt ss
    ESectL e i -> i : allVarsExpr e
    ESectR i e -> i : allVarsExpr e
    EIf e1 e2 e3 -> allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3
    EListish l -> allVarsListish l
    ESign e _ -> allVarsExpr e
    EAt i e -> i : allVarsExpr e
    EUVar _ -> []
    ECon c -> [conIdent c]
    EForall iks e -> map (\ (IdKind i _) -> i) iks ++ allVarsExpr e

allVarsListish :: Listish -> [Ident]
allVarsListish (LList es) = concatMap allVarsExpr es
allVarsListish (LCompr e ss) = allVarsExpr e ++ concatMap allVarsStmt ss
allVarsListish (LFrom e) = allVarsExpr e
allVarsListish (LFromTo e1 e2) = allVarsExpr e1 ++ allVarsExpr e2
allVarsListish (LFromThen e1 e2) = allVarsExpr e1 ++ allVarsExpr e2
allVarsListish (LFromThenTo e1 e2 e3) = allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3

allVarsCaseArm :: ECaseArm -> [Ident]
allVarsCaseArm (p, alts) = allVarsPat p ++ allVarsAlts alts

allVarsStmt :: EStmt -> [Ident]
allVarsStmt astmt =
  case astmt of
    SBind p e -> allVarsPat p ++ allVarsExpr e
    SThen e -> allVarsExpr e
    SLet bs -> concatMap allVarsBind bs

-----------------------------

-- XXX Should use locations in ELit
getSLocExpr :: Expr -> SLoc
getSLocExpr e = head $ filter (not . isNoSLoc) (map getSLocIdent (allVarsExpr e)) ++ [noSLoc]

getSLocCon :: Con -> SLoc
getSLocCon (ConData _ i) = getSLocIdent i
getSLocCon (ConNew i) = getSLocIdent i
getSLocCon _ = noSLoc

setSLocExpr :: SLoc -> Expr -> Expr
setSLocExpr l (EVar i) = EVar (setSLocIdent l i)
setSLocExpr l (ECon c) = ECon (setSLocCon l c)
setSLocExpr l (ELit _ k) = ELit l k
setSLocExpr _ _ = error "setSLocExpr"  -- what other cases do we need?

setSLocCon :: SLoc -> Con -> Con
setSLocCon l (ConData ti i) = ConData ti (setSLocIdent l i)
setSLocCon l (ConNew i) = ConNew (setSLocIdent l i)
setSLocCon _ c = c

errorMessage :: --XHasCallStack =>
                forall a . SLoc -> String -> a
errorMessage loc msg = error $ showSLoc loc ++ ": " ++ msg

----------------

{-
showEModule :: EModule -> String
showEModule am =
  case am of
    EModule i es ds -> "module " ++ i ++ "(\n" ++
      unlines (intersperse "," (map showExportItem es)) ++
      "\n) where\n" ++
      showEDefs ds

showExportItem :: ExportItem -> String
showExportItem ae =
  case ae of
    ExpModule i -> "module " ++ i
    ExpTypeCon i -> i ++ "(..)"
    ExpType i -> i
    ExpValue i -> i
-}

showImportItem :: ImportItem -> String
showImportItem ae =
  case ae of
    ImpTypeCon i -> showIdent i ++ "(..)"
    ImpType i -> showIdent i
    ImpValue i -> showIdent i

showEDef :: EDef -> String
showEDef def =
  case def of
    Data lhs cs -> "data " ++ showLHS lhs ++ " = " ++ intercalate " | " (map showConstr cs)
    Newtype lhs c t -> "newtype " ++ showLHS lhs ++ " = " ++ showIdent c ++ " " ++ showEType t
    Type lhs t -> "type " ++ showLHS lhs ++ " = " ++ showEType t
    Fcn i eqns -> unlines (map (\ (Eqn ps alts) -> showIdent i ++ " " ++ unwords (map showEPat ps) ++ showAlts "=" alts) eqns)
    Sign i t -> showIdent i ++ " :: " ++ showEType t
    Import (ImportSpec q m mm mis) -> "import " ++ (if q then "qualified " else "") ++ showIdent m ++ maybe "" ((" as " ++) . unIdent) mm ++
      case mis of
        Nothing -> ""
        Just (h, is) -> (if h then " hiding" else "") ++ "(" ++ intercalate ", " (map showImportItem is) ++ ")"
    ForImp ie i t -> "foreign import ccall " ++ showString ie ++ " " ++ showIdent i ++ " :: " ++ showEType t
    Infix (a, p) is -> "infix" ++ f a ++ " " ++ showInt p ++ " " ++ intercalate ", " (map showIdent is)
      where f AssocLeft = "l"; f AssocRight = "r"; f AssocNone = ""

showConstr :: Constr -> String
showConstr (Constr i ts) = unwords (showIdent i : map showEType ts)

showLHS :: LHS -> String
showLHS lhs =
  case lhs of
    (f, vs) -> unwords (showIdent f : map showIdKind vs)

showIdKind :: IdKind -> String
showIdKind (IdKind i k) = "(" ++ showIdent i ++ "::" ++ showEKind k ++ ")"

showEDefs :: [EDef] -> String
showEDefs ds = unlines (map showEDef ds)

showAlts :: String -> EAlts -> String
showAlts sep (EAlts alts bs) = showAltsL sep alts ++ showWhere bs

showWhere :: [EBind] -> String
showWhere [] = ""
showWhere bs = "where\n" ++ unlines (map showEBind bs)

showAltsL :: String -> [EAlt] -> String
showAltsL sep [([], e)] = " " ++ sep ++ " " ++ showExpr e
showAltsL sep alts = unlines (map (showAlt sep) alts)

showAlt :: String -> EAlt -> String
showAlt sep (ss, e) = " | " ++ concat (intersperse ", " (map showEStmt ss)) ++ " " ++ sep ++ " " ++ showExpr e

showExpr :: Expr -> String
showExpr ae =
  case ae of
    EVar v -> showIdent v
    EApp _ _ -> showApp [] ae
    EOper e ies -> showExpr (foldl (\ e1 (i, e2) -> EApp (EApp (EVar i) e1) e2) e ies)
    ELam ps e -> "(\\" ++ unwords (map showExpr ps) ++ " -> " ++ showExpr e ++ ")"
    ELit _ i -> showLit i
    ECase e as -> "case " ++ showExpr e ++ " of {\n" ++ unlines (map showCaseArm as) ++ "}"
    ELet bs e -> "let\n" ++ unlines (map showEBind bs) ++ "in " ++ showExpr e
    ETuple es -> "(" ++ intercalate "," (map showExpr es) ++ ")"
    EListish (LList es) -> showList showExpr es
    EDo mn ss -> maybe "do" (\ n -> showIdent n ++ ".do\n") mn ++ unlines (map showEStmt ss)
    ESectL e i -> "(" ++ showExpr e ++ " " ++ showIdent i ++ ")"
    ESectR i e -> "(" ++ showIdent i ++ " " ++ showExpr e ++ ")"
    EIf e1 e2 e3 -> "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
    EListish l -> showListish l
    ESign e t -> showExpr e ++ " :: " ++ showEType t
    EAt i e -> showIdent i ++ "@" ++ showExpr e
    EUVar i -> "a" ++ showInt i
    ECon c -> showCon c
    EForall iks e -> "forall " ++ unwords (map showIdKind iks) ++ " . " ++ showEType e
  where
    showApp as (EApp f a) = showApp (a:as) f
    showApp as (EVar i) | eqString op "->", [a, b] <- as = "(" ++ showExpr a ++ " -> " ++ showExpr b ++ ")"
                        | eqChar (head op) ',' = showExpr (ETuple as)
                        | eqString op "[]", length as == 1 = showExpr (EListish (LList as))
                        where op = unQualString (unIdent i)
    showApp as f = "(" ++ unwords (map showExpr (f:as)) ++ ")"

showListish :: Listish -> String
showListish _ = "<<Listish>>"

showCon :: Con -> String
showCon (ConData _ s) = showIdent s
showCon (ConNew s) = showIdent s
showCon (ConLit l) = showLit l

showLit :: Lit -> String
showLit l =
  case l of
    LInt i -> showInt i
    LDouble d -> '%' : D.showDouble d
    LChar c -> showChar c
    LStr s -> showString s
    LPrim s -> '$' : s
    LForImp s -> '#' : s

showEStmt :: EStmt -> String
showEStmt as =
  case as of
    SBind p e -> showEPat p ++ " <- " ++ showExpr e
    SThen e -> showExpr e
    SLet bs -> "let\n" ++ unlines (map showEBind bs)

showEBind :: EBind -> String
showEBind ab =
  case ab of
    BFcn i eqns -> showEDef (Fcn i eqns)
    BPat p e -> showEPat p ++ " = " ++ showExpr e
    BSign i t -> showIdent i ++ " :: " ++ showEType t

showCaseArm :: ECaseArm -> String
showCaseArm arm =
  case arm of
    (p, alts) -> showEPat p ++ showAlts "->" alts

showEPat :: EPat -> String
showEPat = showExpr

showEType :: EType -> String
showEType = showExpr

showEKind :: EKind -> String
showEKind = showEType
