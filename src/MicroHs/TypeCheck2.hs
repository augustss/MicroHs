module MicroHs.TypeCheck2(module MicroHs.TypeCheck2) where
import Data.IORef
import qualified Data.Map as M
type Name = String

type ErrMsg = String
type TcEnv = M.Map Name Sigma

newtype Tc a = Tc (TcEnv -> IO (Either ErrMsg a))

-- Control flow
check :: Bool -> String -> Tc () -- Type inference can fail
check = undefined

-- The type environment
lookupVar :: Name -> Tc Sigma -- Look up in the envt (may fail)
lookupVar = undefined
extendVarEnv :: Name -> Sigma -- Extend the envt
             -> Tc a -> Tc a
extendVarEnv = undefined
getEnvTypes :: Tc [Sigma] -- Get all types in the envt

-- Instantiation, skolemisation, quantification
instantiate :: Sigma -> Tc Rho
instantiate = undefined
skolemise :: Sigma -> Tc ([TyVar], Rho)
skolemise = undefined
quantify :: [MetaTv] -> Rho -> Tc Sigma
quantify = undefined

-- Unification and fresh type variables
newMetaTyVar :: Tc Tau -- Make (MetaTv tv), where tv is fresh
newMetaTyVar = undefined
newSkolemTyVar :: Tc TyVar -- Make a fresh skolem TyVar
newSkolemTyVar = undefined
unify :: Tau -> Tau -> Tc () -- Unification (may fail)
unify = undefined

-- Free type variables
getMetaTyVars :: [Type] -> Tc [MetaTv]
getMetaTyVars = undefined
getFreeTyVars :: [Type] -> Tc [TyVar]
getFreeTyVars = undefined

newTcRef :: a -> Tc (IORef a)
newTcRef = undefined
readTcRef :: IORef a -> Tc a
readTcRef = undefined
writeTcRef :: IORef a -> a -> Tc ()
writeTcRef = undefined

---------------- Terms -------------------
data Term
  = Var Name -- x
  | Lit Int -- 3
  | App Term Term -- f x
  | Lam Name Term -- \x. x
  | Let Name Term Term -- let x = f y in x+1
  | Ann Term Sigma -- f x :: Int

---------------- Types -------------------
type Sigma = Type
type Rho = Type -- No top-level ForAll
type Tau = Type -- No ForAlls anywhere

data Type
  = ForAll [TyVar] Rho -- Forall type
  | Fun Type Type -- Function type
  | TyCon TyCon -- Type constants
  | TyVar TyVar -- Always bound by a ForAll
  | MetaTv MetaTv -- A meta type variable

data TyVar
  = BoundTv String -- A type variable bound by a ForAll
  | SkolemTv String Uniq -- A skolem constant; the String is
                         -- just to improve error messages
data TyCon = IntT | BoolT

(-->) :: Type -> Type-> Type -- Build a function type
arg --> res = Fun arg res

intType :: Tau
intType = TyCon IntT

--------------------------------------------

data MetaTv = Meta Uniq TyRef
type TyRef = IORef (Maybe Tau)
     -- 'Nothing' means the type variable is not substituted
     -- 'Just ty' means it has been substituted by ty
type Uniq = Int

--------------------------------------------
