module System.IO.Serialize.TypeDescr(
  getTypeDescr, getTypeDescrMD5,
  TypeDescr(..), TypeEnv, TypeInfo(..)
  ) where
import qualified Prelude(); import MiniPrelude
import Data.Data
import Data.Data_Class
import Data.Function((&))
import Data.List
import Data.Ord
import Data.Typeable(typeRepFingerprint)
import System.IO.MD5
import Debug.Trace

newtype TypeInfo = TypeInfo [(ConstrRep, [TypeKey])]
  deriving (Show)

data TypeDescr = TypeDescr TypeKey TypeEnv
  deriving (Show)

type TypeKey = TypeRep
type TypeEnv = [(TypeKey, TypeInfo)]  -- Sorted list of types already seen

-- | Get a descriptor of a type that fully describes the type.
getTypeDescr :: forall a . Data a => a -> TypeDescr
getTypeDescr _ = TypeDescr (typeKey a) (collectTypes a [])
  where a = undefined :: a

getTypeDescrMD5 :: forall a . Data a => a -> MD5CheckSum
getTypeDescrMD5 a =
  case getTypeDescr a of
    TypeDescr rkey renv -> md5Combine $ typeRepFingerprint rkey : concatMap md5ki renv
  where
    md5ki :: (TypeKey, TypeInfo) -> [MD5CheckSum]
    md5ki (key, TypeInfo info) = typeRepFingerprint key : concatMap md5i info
    md5i :: (ConstrRep, [TypeKey]) -> [MD5CheckSum]
    md5i (crep, keys) = md5String (show crep) : map typeRepFingerprint keys

typeKey :: Data a => a -> TypeKey
typeKey = typeOf

-- Recursively build the flat type environment using fully qualified keys
collectTypes :: forall a . Data a => a -> TypeEnv -> TypeEnv
collectTypes a env =
  let key = typeKey a
  in if any ((== key) . fst) env
     then env  -- Already visited; breaks recursive loops
     else
       let dType = dataTypeOf a
           dRep  = dataTypeRep dType

           (cDescrs, finalEnv) =
             case dRep of
               AlgRep constrs ->
                 let env' = insertBy (comparing fst) (key, TypeInfo []) env  -- create dummy entry
                 in  foldr (processConstr a) ([], env') constrs
               _ ->
                 ([], env)  -- Primitives have no constructors
           info = TypeInfo cDescrs
       in  replaceKey key info finalEnv

processConstr :: forall a. Data a
              => a
              -> Constr
              -> ([(ConstrRep, [TypeKey])], TypeEnv)
              -> ([(ConstrRep, [TypeKey])], TypeEnv)
processConstr _ constr (acc, currentEnv) =
  let dummyVal :: a
      dummyVal = fromConstr constr

      (fTypes, nextEnv) = extractFields currentEnv dummyVal
  in ((conrep constr, fTypes) : acc, nextEnv)

extractFields :: Data a => TypeEnv -> a -> ([TypeKey], TypeEnv)
extractFields env val =
  let fKeys    = gmapQ typeKey val
      updaters = gmapQ collectTypes val
      finalEnv = foldl' (&) env updaters
  in  (fKeys, finalEnv)

replaceKey :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
replaceKey k v = map (\(k', v') -> if k' == k then (k, v) else (k', v'))
