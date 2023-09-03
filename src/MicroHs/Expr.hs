module MicroHs.Expr(
  IdentModule,
  EModule(..),
  ExportSpec(..),
  ImportSpec(..),
  EDef(..), showEDefs,
  Expr(..), showExpr,
  Lit(..), showLit,
  EBind(..),
  Eqn(..),
  EStmt(..),
  EAlts(..),
  EAlt,
  ECaseArm,
  EType,
  EPat, patVars, isPVar, isPConApp,
  EKind, kType,
  IdKind(..), idKindIdent,
  LHS,
  Constr,
  ConTyInfo,
  ETypeScheme(..),
  Con(..), conIdent, conArity, eqCon,
  tupleConstr, untupleConstr,
  subst,
  allVarsExpr, allVarsBind,
  getSLocExpr,
  errorMessage
  ) where
import Prelude --Xhiding (Monad(..), Applicative(..), MonadFail(..), Functor(..), (<$>), showString, showChar, showList)
import Data.List
import Data.Maybe
--Ximport Compat
--Ximport GHC.Stack
import MicroHs.Ident

type IdentModule = Ident

----------------------

data EModule = EModule IdentModule [ExportSpec] [EDef]
  --Xderiving (Show, Eq)

data ExportSpec
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
  | Sign Ident ETypeScheme
  | Import ImportSpec
  --Xderiving (Show, Eq)

data ImportSpec = ImportSpec Bool Ident (Maybe Ident)
  --Xderiving (Show, Eq)

data Expr
  = EVar Ident
  | EApp Expr Expr
  | ELam [EPat] Expr
  | ELit SLoc Lit
  | ECase Expr [ECaseArm]
  | ELet [EBind] Expr
  | ETuple [Expr]
  | EList [Expr]
  | EDo (Maybe Ident) [EStmt]
  | ESectL Expr Ident
  | ESectR Ident Expr
  | EIf Expr Expr Expr
  | ECompr Expr [EStmt]
  | ESign Expr EType
  | EAt Ident Expr  -- only in patterns
  -- Only while type checking
  | EUVar Int
  -- Constructors after type checking
  | ECon Con
  --Xderiving (Show, Eq)

data Con
  = ConData ConTyInfo Ident
  | ConNew Ident
  | ConLit Lit
--  | ConTup Int
  --Xderiving(Show, Eq)

conIdent :: --XHasCallStack =>
            Con -> Ident
conIdent (ConData _ i) = i
conIdent (ConNew i) = i
conIdent _ = undefined

conArity :: Con -> Int
conArity (ConData cs i) = fromMaybe undefined $ lookupBy eqIdent i cs
conArity (ConNew _) = 1
conArity (ConLit _) = 0
--conArity (ConTup n) = n

eqCon :: Con -> Con -> Bool
eqCon (ConData _ i) (ConData _ j) = eqIdent i j
eqCon (ConNew    i) (ConNew    j) = eqIdent i j
eqCon (ConLit    l) (ConLit    k) = eqLit   l k
eqCon _             _             = False

data Lit = LInt Int | LChar Char | LStr String | LPrim String
  --Xderiving (Show, Eq)

eqLit :: Lit -> Lit -> Bool
eqLit (LInt x)  (LInt  y) = x == y
eqLit (LChar x) (LChar y) = eqChar x y
eqLit (LStr  x) (LStr  y) = eqString x y
eqLit (LPrim x) (LPrim y) = eqString x y
eqLit _         _         = False

type ECaseArm = (EPat, EAlts)

data EStmt = SBind EPat Expr | SThen Expr | SLet [EBind]
  --Xderiving (Show, Eq)

data EBind = BFcn Ident [Eqn] | BPat EPat Expr
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
type Constr = (Ident, [EType])

-- Expr restricted to
--  * after desugaring: EApp and EVar
--  * before desugaring: EApp, EVar, ETuple, EList
type EType = Expr

{-
validType :: Expr -> Bool
validType ae =
  case ae of
    EVar _ -> True
    EApp f a -> validType f && validType a
    EList es -> length es <= 1 && all validType (take 1 es)
    ETuple es -> all validType es
    _ -> False
-}

data ETypeScheme = ETypeScheme [IdKind] EType
  --Xderiving (Show, Eq)

data IdKind = IdKind Ident EKind
  --Xderiving (Show, Eq)

idKindIdent :: IdKind -> Ident
idKindIdent (IdKind i _) = i

type EKind = EType

kType :: EKind
kType = EVar (Ident noSLoc "Primitives.Type")

tupleConstr :: Int -> Ident
tupleConstr n = mkIdent (replicate (n - 1) ',')

untupleConstr :: Ident -> Int
untupleConstr i = length (unIdent i) + 1

---------------------------------

-- Enough to handle subsitution in types
subst :: [(Ident, Expr)] -> Expr -> Expr
subst s =
  let
    sub ae =
      case ae of
        EVar i -> fromMaybe ae $ lookupBy eqIdent i s
        EApp f a -> EApp (sub f) (sub a)
        EUVar _ -> ae
        _ -> error "subst unimplemented"
  in sub

allVarsBind :: EBind -> [Ident]
allVarsBind abind =
  case abind of
    BFcn i eqns -> i : concatMap allVarsEqn eqns
    BPat p e -> allVarsPat p ++ allVarsExpr e

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
    ELam ps e -> concatMap allVarsPat ps ++ allVarsExpr e
    ELit _ _ -> []
    ECase e as -> allVarsExpr e ++ concatMap allVarsCaseArm as
    ELet bs e -> concatMap allVarsBind bs ++ allVarsExpr e
    ETuple es -> concatMap allVarsExpr es
    EList es -> concatMap allVarsExpr es
    EDo mi ss -> maybe [] (:[]) mi ++ concatMap allVarsStmt ss
    ESectL e i -> i : allVarsExpr e
    ESectR i e -> i : allVarsExpr e
    EIf e1 e2 e3 -> allVarsExpr e1 ++ allVarsExpr e2 ++ allVarsExpr e3
    ECompr e ss -> allVarsExpr e ++ concatMap allVarsStmt ss
    ESign e _ -> allVarsExpr e
    EAt i e -> i : allVarsExpr e
    EUVar _ -> []
    ECon c -> [conIdent c]

allVarsCaseArm :: ECaseArm -> [Ident]
allVarsCaseArm (p, alts) = allVarsPat p ++ allVarsAlts alts

allVarsStmt :: EStmt -> [Ident]
allVarsStmt astmt =
  case astmt of
    SBind p e -> allVarsPat p ++ allVarsExpr e
    SThen e -> allVarsExpr e
    SLet bs -> concatMap allVarsBind bs

-- XXX Should use locations in ELit
getSLocExpr :: Expr -> SLoc
getSLocExpr e = head $ map getSLocIdent (allVarsExpr e) ++ [noSLoc]

errorMessage :: --XHasCallStack =>
                forall a . SLoc -> String -> a
errorMessage loc msg = error $ showSLoc loc ++ msg

----------------

{-
showEModule :: EModule -> String
showEModule am =
  case am of
    EModule i es ds -> "module " ++ i ++ "(\n" ++
      unlines (intersperse "," (map showExportSpec es)) ++
      "\n) where\n" ++
      showEDefs ds

showExportSpec :: ExportSpec -> String
showExportSpec ae =
  case ae of
    ExpModule i -> "module " ++ i
    ExpTypeCon i -> i ++ "(..)"
    ExpType i -> i
    ExpValue i -> i
-}

showEDef :: EDef -> String
showEDef def =
  case def of
    Data lhs _ -> "data " ++ showLHS lhs ++ " = ..."
    Newtype lhs c t -> "newtype " ++ showLHS lhs ++ " = " ++ showIdent c ++ " " ++ showEType t
    Type lhs t -> "type " ++ showLHS lhs ++ " = " ++ showEType t
    Fcn i eqns -> unlines (map (\ (Eqn ps alts) -> showIdent i ++ " " ++ unwords (map showEPat ps) ++ showAlts "=" alts) eqns)
    Sign i t -> showIdent i ++ " :: " ++ showETypeScheme t
    Import (ImportSpec q m mm) -> "import " ++ (if q then "qualified " else "") ++ showIdent m ++ maybe "" ((" as " ++) . unIdent) mm

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
    ELam ps e -> "(\\" ++ unwords (map showExpr ps) ++ " -> " ++ showExpr e ++ ")"
    ELit _ i -> showLit i
    ECase e as -> "case " ++ showExpr e ++ " of {\n" ++ unlines (map showCaseArm as) ++ "}"
    ELet bs e -> "let\n" ++ unlines (map showEBind bs) ++ "in " ++ showExpr e
    ETuple es -> "(" ++ intercalate "," (map showExpr es) ++ ")"
    EList es -> showList showExpr es
    EDo mn ss -> maybe "do" (\ n -> showIdent n ++ ".do\n") mn ++ unlines (map showEStmt ss)
    ESectL e i -> "(" ++ showExpr e ++ " " ++ showIdent i ++ ")"
    ESectR i e -> "(" ++ showIdent i ++ " " ++ showExpr e ++ ")"
    EIf e1 e2 e3 -> "if " ++ showExpr e1 ++ " then " ++ showExpr e2 ++ " else " ++ showExpr e3
    ECompr _ _ -> "ECompr"
    ESign e t -> showExpr e ++ " :: " ++ showEType t
    EAt i e -> showIdent i ++ "@" ++ showExpr e
    EUVar i -> "a" ++ showInt i
    ECon c -> showCon c
  where
    showApp as (EApp f a) = showApp (as ++ [a]) f
    showApp as (EVar i) | eqString op "->", [a, b] <- as = "(" ++ showExpr a ++ " -> " ++ showExpr b ++ ")"
                        | eqChar (head op) ',' = showExpr (ETuple as)
                        | eqString op "[]", length as == 1 = showExpr (EList as)
                        where op = unQualString (unIdent i)
    showApp as f = "(" ++ unwords (map showExpr (f:as)) ++ ")"

showCon :: Con -> String
showCon (ConData _ s) = showIdent s
showCon (ConNew s) = showIdent s
showCon (ConLit l) = showLit l

showLit :: Lit -> String
showLit l =
  case l of
    LInt i -> showInt i
    LChar c -> showChar c
    LStr s -> showString s
    LPrim s -> '$':s

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

showETypeScheme :: ETypeScheme -> String
showETypeScheme ts =
  case ts of
    ETypeScheme vs t ->
      if null vs
      then showEType t
      else unwords ("forall" : map showIdKind vs ++ [".", showEType t])

