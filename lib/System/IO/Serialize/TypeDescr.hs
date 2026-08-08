module System.IO.Serialize.TypeDescr(getTypeDescr, TypeDescr(..), TypeEnv, TypeInfo) where

import Data.Data
import Data.Data_Class
import Data.Function((&))
import Data.List
import Data.Ord

data TypeInfo = TypeInfo [(ConstrRep, [TypeKey])]
  deriving (Show)

data TypeDescr = TypeDescr TypeKey TypeEnv
  deriving (Show)

type TypeKey = TypeRep
type TypeEnv = [(TypeKey, TypeInfo)]  -- Sorted list of types already seen

-- | Get a descriptor of a type that fully describes the type.
getTypeDescr :: Data a => a -> TypeDescr
getTypeDescr dummy =
  let rootKey = typeKey dummy
      env     = collectTypes dummy []
  in TypeDescr rootKey env

typeKey :: Data a => a -> TypeKey
typeKey = typeOf

-- Recursively build the flat type environment using fully qualified keys
collectTypes :: Data a => a -> TypeEnv -> TypeEnv
collectTypes dummy env =
  let key = typeKey dummy
  in if any ((== key) . fst) env
     then env  -- Already visited; breaks recursive loops
     else
       let dType = dataTypeOf dummy
           dRep  = dataTypeRep dType

           (cDescrs, finalEnv) =
             case dRep of
               AlgRep constrs ->
                 let env' = insertBy (comparing fst) (key, TypeInfo []) env  -- create dummy entry
                 in  foldr (processConstr dummy) ([], env') constrs
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
