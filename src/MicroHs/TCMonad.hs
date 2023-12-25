{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  module MicroHs.TCMonad,
  get, put, gets,
  ) where
import Prelude
import Data.Functor.Identity
import GHC.Stack
import Control.Applicative
import Control.Monad.State.Strict
import Data.Functor
import qualified Data.IntMap as IM
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.Expr
import Compat

-----------------------------------------------
-- TC

type TC s a = State s a

tcRun :: forall s a . TC s a -> s -> (a, s)
tcRun = runState

tcError :: forall s a .
           HasCallStack =>
           SLoc -> String -> TC s a
tcError = errorMessage

instance MonadFail Identity where fail = error

-----------------------------------------------
-- Symtab

data SymTab a = SymTab (M.Map [a]) [(Ident, a)]
--  deriving(Show)

instance forall a . Show a => Show (SymTab a) where
  show (SymTab g l) = unlines $
    ("Locals:"  : map (("  " ++) . show) l) ++
    ("Globals:" : map (("  " ++) . show) (M.toList g))
  
mapMSymTab :: forall a . (a -> T a) -> SymTab a -> T (SymTab a)
mapMSymTab f (SymTab g l) = do
  g' <- M.mapM (mapM f) g
  l' <- mapM (\ (i, a) -> (i,) <$> f a) l
  return $ SymTab g' l'

stLookup :: String -> Ident -> SymTab Entry -> Either String Entry
stLookup msg i (SymTab genv lenv) =
  case lookup i lenv of
    Just e -> Right e
    Nothing ->
      case M.lookup i genv of
        Just [e] -> Right e
        Just es  -> Left $ "ambiguous " ++ msg ++ ": " ++ showIdent i ++ " " ++ showListS (showIdent . getAppCon) [ e | Entry e _ <- es ]
        Nothing  -> Left $ "undefined " ++ msg ++ ": " ++ showIdent i
                           -- ++ "\n" ++ show lenv ++ "\n" ++ show genv

stFromListWith :: forall a . ([a] -> [a] -> [a]) -> [(Ident, [a])] -> SymTab a
stFromListWith comb ias = SymTab (M.fromListWith comb ias) []

stFromList :: forall a . [(Ident, [a])] -> SymTab a
stFromList ias = SymTab (M.fromList ias) []

stElemsLcl :: forall a . SymTab a -> [a]
stElemsLcl (SymTab _genv lenv) = map snd lenv

stInsertLcl :: forall a . Ident -> a -> SymTab a -> SymTab a
stInsertLcl i a (SymTab genv lenv) = SymTab genv ((i,a) : lenv)

-- XXX Use insertWith to follow Haskell semantics.
stInsertGlb :: forall a . Ident -> [a] -> SymTab a -> SymTab a
stInsertGlb i as (SymTab genv lenv) = SymTab (M.insert i as genv) lenv

-----------------------------------------------
-- Entry

-- Symbol table entry for symbol i.
data Entry = Entry
  Expr             -- convert (EVar i) to this expression; sometimes just (EVar i)
  EType            -- type/kind of identifier
--  deriving(Show)

instance Show Entry where
  showsPrec _ (Entry e t) = showsPrec 0 e . showString " :: " . showsPrec 0 t

instance Eq Entry where
  Entry x _ == Entry y _  =  getIdent x == getIdent y


entryType :: Entry -> EType
entryType (Entry _ t) = t

-----------------------------------------------

-----------------------------------------------
-- Tables

type ValueTable = SymTab Entry     -- type of value identifiers, used during type checking values
type TypeTable  = SymTab Entry     -- kind of type  identifiers, used during kind checking types
type KindTable  = SymTab Entry     -- sort of kind  identifiers, used during sort checking kinds
type SynTable   = M.Map EType      -- body of type synonyms
type FixTable   = M.Map Fixity     -- precedence and associativity of operators
type AssocTable = M.Map [Ident]    -- maps a type identifier to its associated constructors/selectors/methods
type ClassTable = M.Map ClassInfo  -- maps a class identifier to its associated information
type InstTable  = M.Map InstInfo   -- indexed by class name
type MetaTable  = [(Ident, EConstraint)]  -- instances with unification variables
type Constraints= [(Ident, EConstraint)]
type Defaults   = [EType]          -- Current defaults

-- To make type checking fast it is essential to solve constraints fast.
-- The naive implementation of InstInfo would be [InstDict], but
-- that is slow.
-- Instead, the data structure is specialized
--  * For single parameter type classes for atomic types, e.g., Eq Int
--    we use the type name (i.e., Int) to index into a map that gives
--    the dictionary directly.  This map is also used for dictionary arguments
--    of type, e.g., Eq a.
--  * NOT IMPLEMENTED: look up by type name of the left-most type
--  * As a last resort, just look through dictionaries.
data InstInfo = InstInfo
       (M.Map Expr)               -- map for direct lookup of atomic types
       [InstDict]                 -- slow path
       [IFunDep]
  deriving (Show)

-- This is the dictionary expression, instance variables, instance context,
-- and instance.
type InstDictC  = (Expr, [IdKind], [EConstraint], EConstraint, [IFunDep])
-- This is the dictionary expression, instance context, and types.
-- An instance (C T1 ... Tn) has the type list [T1,...,Tn]
-- The types and constraint have their type variables normalized to EUVar (-1), EUVar (-2), etc
type InstDict   = (Expr, [EConstraint], [EType])

-- All known type equalities, contains the transitive&commutative closure.
type TypeEqTable = [(EType, EType)]

-----------------------------------------------
-- TCState

data TCState = TC
  IdentModule           -- current module name
  Int                   -- unique number
  FixTable              -- fixities, indexed by QIdent
  TypeTable             -- type symbol table
  SynTable              -- synonyms, indexed by QIdent
  ValueTable            -- value symbol table
  AssocTable            -- values associated with a type, indexed by QIdent
  (IM.IntMap EType)     -- mapping from unique id to type
  TCMode                -- pattern, value, or type
  ClassTable            -- class info, indexed by QIdent
  (InstTable,           -- instances
   MetaTable,           -- instances with unification variables
   TypeEqTable)         -- type equalities
  Constraints           -- constraints that have to be solved
  Defaults              -- current defaults
  deriving (Show)

-----------------------------------------------
-- TCState access

typeTable :: TCState -> TypeTable
typeTable (TC _ _ _ tt _ _ _ _ _ _ _ _ _) = tt

valueTable :: TCState -> ValueTable
valueTable (TC _ _ _ _ _ vt _ _ _ _ _ _ _) = vt

synTable :: TCState -> SynTable
synTable (TC _ _ _ _ st _ _ _ _ _ _ _ _) = st

fixTable :: TCState -> FixTable
fixTable (TC _ _ ft _ _ _ _ _ _ _ _ _ _) = ft

assocTable :: TCState -> AssocTable
assocTable (TC _ _ _ _ _ _ ast _ _ _ _ _ _) = ast

uvarSubst :: TCState -> IM.IntMap EType
uvarSubst (TC _ _ _ _ _ _ _ sub _ _ _ _ _) = sub

moduleName :: TCState -> IdentModule
moduleName (TC mn _ _ _ _ _ _ _ _ _ _ _ _) = mn

classTable :: TCState -> ClassTable
classTable (TC _ _ _ _ _ _ _ _ _ ct _ _ _) = ct

tcMode :: TCState -> TCMode
tcMode (TC _ _ _ _ _ _ _ _ m _ _ _ _) = m

instTable :: TCState -> InstTable
instTable (TC _ _ _ _ _ _ _ _ _ _ (is, _, _) _ _) = is

metaTable :: TCState -> MetaTable
metaTable (TC _ _ _ _ _ _ _ _ _ _ (_, ms, _) _ _) = ms

typeEqTable :: TCState -> TypeEqTable
typeEqTable (TC _ _ _ _ _ _ _ _ _ _ (_, _, es) _ _) = es

ctxTables :: TCState -> (InstTable, MetaTable, TypeEqTable)
ctxTables (TC _ _ _ _ _ _ _ _ _ _ ct _ _) = ct

constraints :: TCState -> Constraints
constraints (TC _ _ _ _ _ _ _ _ _ _ _ e _) = e

defaults :: TCState -> Defaults
defaults (TC _ _ _ _ _ _ _ _ _ _ _ _ ds) = ds

putValueTable :: ValueTable -> T ()
putValueTable venv = do
  TC mn n fx tenv senv _ ast sub m cs is es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = do
  TC mn n fx _ senv venv ast sub m cs is es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putSynTable :: SynTable -> T ()
putSynTable senv = do
  TC mn n fx tenv _ venv ast sub m cs is es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putUvarSubst :: IM.IntMap EType -> T ()
putUvarSubst sub = do
  TC mn n fx tenv senv venv ast _ m cs is es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putTCMode :: TCMode -> T ()
putTCMode m = do
  TC mn n fx tenv senv venv ast sub _ cs is es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putInstTable :: InstTable -> T ()
putInstTable is = do
  TC mn n fx tenv senv venv ast sub m cs (_,ms,eqs) es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs (is,ms,eqs) es ds)

putMetaTable :: MetaTable -> T ()
putMetaTable ms = do
  TC mn n fx tenv senv venv ast sub m cs (is,_,eqs) es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs (is,ms,eqs) es ds)

putTypeEqTable :: TypeEqTable -> T ()
putTypeEqTable eqs = do
  TC mn n fx tenv senv venv ast sub m cs (is,ms,_) es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs (is,ms,eqs) es ds)

putCtxTables :: (InstTable, MetaTable, TypeEqTable) -> T ()
putCtxTables ct = do
  TC mn n fx tenv senv venv ast sub m cs _ es ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs ct es ds)

putConstraints :: Constraints -> T ()
putConstraints es = do
  TC mn n fx tenv senv venv ast sub m cs is _ ds <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

putDefaults :: Defaults -> T ()
putDefaults ds = do
  TC mn n fx tenv senv venv ast sub m cs is es _ <- get
  put (TC mn n fx tenv senv venv ast sub m cs is es ds)

-----------------------------------------------
-- TCMode

-- What are we checking
data TCMode
  = TCExpr          -- doing type checking
  | TCType          -- doing kind checking
  | TCKind          -- doing sort checking
  | TCSort
  --deriving (Show)

instance Show TCMode where
  show TCExpr = "TCExpr"
  show TCType = "TCType"
  show TCKind = "TCKind"
  show TCSort = "TCSort"

instance Enum TCMode where
  succ TCExpr = TCType
  succ TCType = TCKind
  succ TCKind = TCSort
  succ TCSort = error "succ TCSort"
  toEnum = undefined
  fromEnum = undefined

instance Ord TCMode where
  TCExpr <= _       =  True

  TCType <= TCExpr  =  False
  TCType <= _       =  True

  TCKind <= TCExpr  =  False
  TCKind <= TCType  =  False
  TCKind <= _       =  True

  TCSort <= TCSort  =  True
  TCSort <= _       =  False

instance Eq TCMode where
  x == y  =  x <= y && y <= x

assertTCMode :: forall a . HasCallStack => (TCMode -> Bool) -> T a -> T a
--assertTCMode _ ta | usingMhs = ta
assertTCMode p ta = do
  tcm <- gets tcMode
  if p tcm then ta else error $ "assertTCMode: expected=" ++ show (filter p [TCExpr,TCType,TCKind]) ++ ", got=" ++ show tcm

-----------------------------------------------

type ClassInfo = ([IdKind], [EConstraint], EType, [Ident], [IFunDep])  -- class tyvars, superclasses, class kind, methods, fundeps
type IFunDep = ([Bool], [Bool])           -- the length of the lists is the number of type variables

type T a = TC TCState a

type Typed a = (a, EType)

getIdent :: Expr -> Ident
getIdent ae =
  case ae of
    EVar i -> i
    ECon c -> conIdent c
    _ -> error "getIdent"

getAppCon :: EType -> Ident
getAppCon (EVar i) = i
getAppCon (EApp f _) = getAppCon f
getAppCon _ = error "getAppCon"

