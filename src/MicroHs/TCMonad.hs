{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-imports -Wno-unused-imports #-}
module MicroHs.TCMonad(
  module MicroHs.TCMonad,
  get, put, gets, modify,
  ) where
import Prelude
import Data.Functor.Identity
import GHC.Stack
import Control.Applicative
import Control.Monad.State.Strict
import Data.Functor
import qualified Data.IntMap as IM
import Data.List(nub)
import MicroHs.Expr
import MicroHs.Ident
import qualified MicroHs.IdentMap as M
import MicroHs.SymTab

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
-- Tables

type ValueTable = SymTab           -- type of value identifiers, used during type checking values
type TypeTable  = SymTab           -- kind of type  identifiers, used during kind checking types
type KindTable  = SymTab           -- sort of kind  identifiers, used during sort checking kinds
type SynTable   = M.Map EType      -- body of type synonyms
type FixTable   = M.Map Fixity     -- precedence and associativity of operators
type AssocTable = M.Map [Ident]    -- maps a type identifier to its associated constructors/selectors/methods
type ClassTable = M.Map ClassInfo  -- maps a class identifier to its associated information
type InstTable  = M.Map InstInfo   -- indexed by class name
type MetaTable  = [(Ident, EConstraint)]  -- instances with unification variables
type Constraints= [(Ident, EConstraint)]
type ArgDicts   = [(Ident, EConstraint)]  -- dictionary arguments
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
--  deriving (Show)

-- This is the dictionary expression, instance variables, instance context,
-- and instance.
type InstDictC  = (Expr, [IdKind], [EConstraint], EConstraint, [IFunDep])
-- This is the dictionary expression, instance context, and types.
-- An instance (C T1 ... Tn) has the type list [T1,...,Tn]
-- The types and constraint have their type variables normalized to EUVar (-1), EUVar (-2), etc
type InstDict   = (Expr, [EConstraint], [EType])

-- All known type equalities, contains the transitive&commutative closure.
type TypeEqTable = [(EType, EType)]

type ClassInfo = ([IdKind], [EConstraint], EType, [Ident], [IFunDep])  -- class tyvars, superclasses, class kind, methods, fundeps
type IFunDep = ([Bool], [Bool])           -- the length of the lists is the number of type variables

-----------------------------------------------
-- TCState
data TCState = TC {
  moduleName  :: IdentModule,           -- current module name
  unique      :: Int,                   -- unique number
  fixTable    :: FixTable,              -- fixities, indexed by QIdent
  typeTable   :: TypeTable,             -- type symbol table
  synTable    :: SynTable,              -- synonyms, indexed by QIdent
  valueTable  :: ValueTable,            -- value symbol table
  assocTable  :: AssocTable,            -- values associated with a type, indexed by QIdent
  uvarSubst   :: (IM.IntMap EType),     -- mapping from unique id to type
  tcMode      :: TCMode,                -- pattern, value, or type
  classTable  :: ClassTable,            -- class info, indexed by QIdent
  ctxTables   :: (InstTable,            -- instances
                  MetaTable,            -- instances with unification variables
                  TypeEqTable,          -- type equalities
                  ArgDicts              -- dictionary arguments
                 ),             
  constraints :: Constraints,           -- constraints that have to be solved
  defaults    :: Defaults               -- current defaults
  }

instTable :: TCState -> InstTable
instTable tc = case ctxTables tc of (x,_,_,_) -> x

metaTable :: TCState -> MetaTable
metaTable tc = case ctxTables tc of (_,x,_,_) -> x

typeEqTable :: TCState -> TypeEqTable
typeEqTable tc = case ctxTables tc of (_,_,x,_) -> x

argDicts :: TCState -> ArgDicts
argDicts tc = case ctxTables tc of (_,_,_,x) -> x


putValueTable :: ValueTable -> T ()
putValueTable venv = modify $ \ ts -> ts{ valueTable = venv }

putTypeTable :: TypeTable -> T ()
putTypeTable tenv = modify $ \ ts -> ts{ typeTable = tenv }

putSynTable :: SynTable -> T ()
putSynTable senv = modify $ \ ts -> ts{ synTable = senv }

putUvarSubst :: IM.IntMap EType -> T ()
putUvarSubst sub = modify $ \ ts -> ts{ uvarSubst = sub }

putTCMode :: TCMode -> T ()
putTCMode m = modify $ \ ts -> ts{ tcMode = m }

putInstTable :: InstTable -> T ()
putInstTable is = do
  (_,ms,eqs,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putMetaTable :: MetaTable -> T ()
putMetaTable ms = do
  (is,_,eqs,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putTypeEqTable :: TypeEqTable -> T ()
putTypeEqTable eqs = do
  (is,ms,_,ads) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putArgDicts :: ArgDicts -> T ()
putArgDicts ads = do
  (is,ms,eqs,_) <- gets ctxTables
  modify $ \ ts -> ts{ ctxTables = (is,ms,eqs,ads) }

putCtxTables :: (InstTable, MetaTable, TypeEqTable, ArgDicts) -> T ()
putCtxTables ct = modify $ \ ts -> ts{ ctxTables = ct }

putConstraints :: Constraints -> T ()
putConstraints es = modify $ \ ts -> ts{ constraints = es }

putDefaults :: Defaults -> T ()
putDefaults ds = modify $ \ ts -> ts{ defaults = ds }


type TRef = Int

newUniq :: T TRef
newUniq = do
  ts <- get
  let n' = n + 1
      n = unique ts
  put $ seq n' $ ts{ unique = n' }
  return n

-----------------------------------------------
-- TCMode

-- What are we checking
data TCMode
  = TCExpr          -- doing type checking
  | TCType          -- doing kind checking
  | TCKind          -- doing sort checking
  | TCSort          -- doing tier checking
  deriving (Show, Eq, Ord)

instance Enum TCMode where
  succ TCExpr = TCType
  succ TCType = TCKind
  succ TCKind = TCSort
  succ TCSort = error "succ TCSort"
  toEnum = undefined
  fromEnum = undefined

assertTCMode :: forall a . HasCallStack => (TCMode -> Bool) -> T a -> T a
--assertTCMode _ ta | usingMhs = ta
assertTCMode p ta = do
  tcm <- gets tcMode
  if p tcm then ta else error $ "assertTCMode: expected=" ++ show (filter p [TCExpr,TCType,TCKind]) ++ ", got=" ++ show tcm

-----------------------------------------------

type T a = TC TCState a

type Typed a = (a, EType)

getAppCon :: HasCallStack => EType -> Ident
getAppCon (EVar i) = i
getAppCon (ECon i) = conIdent i
getAppCon (EApp f _) = getAppCon f
getAppCon e = error $ "getAppCon: " ++ show e

-----------------------------------------------

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

addConstraints :: [EConstraint] -> EType -> EType
addConstraints []  t = t
addConstraints cs  t = tupleConstraints cs `tImplies` t

tupleConstraints :: [EConstraint] -> EConstraint
tupleConstraints []  = error "tupleConstraints"
tupleConstraints [c] = c
tupleConstraints cs  = tApps (tupleConstr noSLoc (length cs)) cs

-----------------------------------------------

builtinLoc :: SLoc
builtinLoc = SLoc "builtin" 0 0

tConI :: SLoc -> String -> EType
tConI loc = tCon . mkIdentSLoc loc

tCon :: Ident -> EType
tCon = EVar

tVarK :: IdKind -> EType
tVarK (IdKind i _) = EVar i

tApp :: EType -> EType -> EType
tApp = EApp

tApps :: Ident -> [EType] -> EType
tApps i ts = eApps (tCon i) ts

tArrow :: EType -> EType -> EType
tArrow a r = tApp (tApp (tConI builtinLoc "Primitives.->") a) r

tImplies :: EType -> EType -> EType
tImplies a r = tApp (tApp (tConI builtinLoc "Primitives.=>") a) r
