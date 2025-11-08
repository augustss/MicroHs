module Data.Typeable (
  Typeable(..),
  TypeRep,
  typeOf,
  cast,
  eqT,
  gcast, gcast1, gcast2,
  TyCon,
  tyConModule,
  tyConName,
  mkTyCon,
  _mkTyCon,
  mkTyConApp,
  mkAppTy,
  mkFunTy,
  splitTyConApp,
  funResultTy,
  typeRepTyCon,
  typeRepArgs,
  ) where
import qualified Prelude(); import MiniPrelude
import Primitives
import Data.Integer
import Data.List.NonEmpty(NonEmpty)
import Data.Proxy
import Data.Ratio
import Data.Type.Equality
import Data.Void
import System.IO.MD5
import Unsafe.Coerce

type  Typeable :: forall k . k -> Constraint
class Typeable a where
  typeRep :: forall proxy . proxy a -> TypeRep

typeOf :: forall a . Typeable a => a -> TypeRep
typeOf _ = typeRep (Proxy :: Proxy a)

-----------------

data TypeRep = TypeRep MD5CheckSum TyCon [TypeRep]

-- Compare keys for equality
instance Eq TypeRep where
  TypeRep k1 _ _ == TypeRep k2 _ _  =  k1 == k2

instance Ord TypeRep where
  TypeRep k1 _ _ <= TypeRep k2 _ _  =  k1 <= k2

instance Show TypeRep where
  showsPrec p (TypeRep _ c []) = showsPrec 11 c
  showsPrec p (TypeRep _ c [a,b]) | c == funTc = showParen (p > 5) $ showsPrec 6 a . showString " -> " . showsPrec 5 b
  showsPrec p (TypeRep _ c [a]) | tyConName c == "[]" = showString "[" . shows a . showString "]"
  showsPrec p (TypeRep _ c ts) | head (tyConName c) == ',' = showParen True $ comma (map shows ts)
                               | otherwise = showParen (p > 11) $ showsPrec 11 c . foldr (\ t s -> showChar ' ' . showsPrec 12 t . s) id ts
    where comma [] = undefined
          comma [s] = s
          comma (s:ss) = s . showString "," . comma ss

typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep _ tc _) = tc

typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ _ args) = args

trMd5 :: TypeRep -> MD5CheckSum
trMd5 (TypeRep md5 _ _) = md5

mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy (TypeRep _ tc trs) tr = mkTyConApp tc (trs ++ [tr])

mkTyConApp :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp tc@(TyCon cmd5 _ _) trs = TypeRep md5 tc trs
  where md5 = md5Combine $ cmd5 : map trMd5 trs

_mkTyCon :: String -> String -> a -> TypeRep
_mkTyCon m n _ = mkTyConApp (mkTyCon m n) []

-----------------

data TyCon = TyCon MD5CheckSum String String

instance Eq TyCon where
  TyCon k1 _ _ == TyCon k2 _ _  =  k1 == k2

instance Ord TyCon where
  TyCon k1 _ _ <= TyCon k2 _ _  =  k1 <= k2

instance Show TyCon where
  showsPrec _ (TyCon _ m n) = showString n

tyConModule :: TyCon -> String
tyConModule (TyCon _ m _) = m

tyConName :: TyCon -> String
tyConName (TyCon _ _ n) = n

mkTyCon :: String -> String -> TyCon
mkTyCon m n = TyCon md5 m n
  where md5 = md5String $ show $ m ++ "." ++ n

mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkTyConApp funTc [f, a]

splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp (TypeRep _ tc trs) = (tc, trs)

funTc :: TyCon
funTc = mkTyCon "Primitives" "->"

funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy trFun trArg
  = case splitTyConApp trFun of
      (tc, [t1, t2]) | tc == funTc && t1 == trArg -> Just t2
      _ -> Nothing

-----------------

cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
           then Just $ unsafeCoerce x
           else Nothing

eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
      then Just $ unsafeCoerce (Refl :: () :~: ())
      else Nothing

gcast :: forall c t t' .
         (Typeable t, Typeable t') => c t -> Maybe (c t')
gcast x =
  case eqT :: Maybe (t :~: t') of
    Just Refl -> Just x
    Nothing -> Nothing

gcast1 :: forall c t t' a .
          (Typeable t, Typeable t') => c (t a) -> Maybe (c (t' a))
gcast1 x =
  case eqT :: Maybe (t :~: t') of
    Just Refl -> Just x
    Nothing -> Nothing

gcast2 :: forall c t t' a b .
          (Typeable t, Typeable t') => c (t a b) -> Maybe (c (t' a b))
gcast2 x =
  case eqT :: Maybe (t :~: t') of
    Just Refl -> Just x
    Nothing -> Nothing

-----------------

-- Primitive types
deriving instance Typeable AnyType
deriving instance Typeable Char
deriving instance Typeable Double
deriving instance Typeable Float
deriving instance Typeable ForeignPtr
deriving instance Typeable Int
deriving instance Typeable Int64
deriving instance Typeable IO
deriving instance Typeable IOArray
deriving instance Typeable MVar
deriving instance Typeable Ptr
deriving instance Typeable FunPtr
deriving instance Typeable ThreadId
deriving instance Typeable Weak
deriving instance Typeable Word
deriving instance Typeable Word64

-- Types too basic to have working deriving
deriving instance Typeable Bool
deriving instance Typeable Integer
deriving instance Typeable []
deriving instance Typeable Maybe
deriving instance Typeable Ordering

prim :: forall a . String -> a -> TypeRep
prim n = _mkTyCon "Primitives" n

-- Builtin types
instance Typeable (->)        where typeRep = prim                          "->"
instance Typeable ()          where typeRep = _mkTyCon "Data.Tuple"          "()"
instance Typeable (,)         where typeRep = _mkTyCon "Data.Tuple"          ","
instance Typeable (,,)        where typeRep = _mkTyCon "Data.Tuple"          ",,"
instance Typeable (,,,)       where typeRep = _mkTyCon "Data.Tuple"          ",,,"
instance Typeable (,,,,)      where typeRep = _mkTyCon "Data.Tuple"          ",,,,"

-- Type application
instance (Typeable f, Typeable a) => Typeable (f a) where
  typeRep _ = mkAppTy (typeRep (Proxy :: Proxy f)) (typeRep (Proxy :: Proxy a))
