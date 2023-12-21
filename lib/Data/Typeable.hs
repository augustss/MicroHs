module Data.Typeable (
  Typeable(..),
  TypeRep,
  typeOf,
  cast,
  TyCon,
  tyConModule,
  tyConName,
  mkTyCon,
  mkTyConApp,
  mkAppTy,
  mkFunTy,
  splitTyConApp,
  funResultTy,
  typeRepTyCon,
  typeRepArgs,

{-
        -- * For backwards compatibility
        typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7,
        Typeable1, Typeable2, Typeable3, Typeable4, Typeable5, Typeable6,
        Typeable7,
        -- * Type-safe cast
        eqT,
        gcast,                  -- a generalisation of cast

        -- * Type representations
        showsTypeRep,


        -- * Construction of type representations
        -- mkTyCon,        -- :: String  -> TyCon
-}
  ) where
import Primitives
import Prelude
import Data.Proxy
import System.IO.MD5
import Unsafe.Coerce

class Typeable a where
  typeRep :: forall (proxy :: Type -> Type) . proxy a -> TypeRep

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
  showsPrec p (TypeRep _ c ts) = showParen (p > 11) $ showsPrec 11 c . foldr (\ t s -> showChar ' ' . showsPrec 11 t . s) id ts

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

-----------------

data TyCon = TyCon MD5CheckSum String String

instance Eq TyCon where
  TyCon k1 _ _ == TyCon k2 _ _  =  k1 == k2

instance Ord TyCon where
  TyCon k1 _ _ <= TyCon k2 _ _  =  k1 <= k2

instance Show TyCon where
  showsPrec _ (TyCon _ m n) = showString m . showChar '.' . showString n

tyConModule :: TyCon -> String
tyConModule (TyCon _ m _) = m

tyConName :: TyCon -> String
tyConName (TyCon _ _ n) = n

mkTyCon :: String -> String -> TyCon
mkTyCon m n = TyCon md5 m n
  where md5 = md5String $ show $ m ++ "." ++ n

mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkTyConApp funTc [f,a]

splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp (TypeRep _ tc trs) = (tc,trs)

funTc :: TyCon
funTc = mkTyCon "Primitives" "->"

funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy trFun trArg
  = case splitTyConApp trFun of
      (tc, [t1,t2]) | tc == funTc && t1 == trArg -> Just t2
      _ -> Nothing

-----------------

cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
           then Just $ unsafeCoerce x
           else Nothing

{-
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
      then Just $ unsafeCoerce Refl
      else Nothing

gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = fmap (\Refl -> x) (eqT :: Maybe (a :~: b))
-}

-----------------

nullary :: forall a . String -> String -> a -> TypeRep
nullary m n _ = mkTyConApp (mkTyCon m n) []

prim :: forall a . String -> a -> TypeRep
prim n = nullary "Primitives" n

instance Typeable ()     where typeRep = nullary "Data.Tuple" "()"
instance Typeable Any    where typeRep = prim "Any"
instance Typeable Bool   where typeRep = nullary "Data.Bool_Type" "Char"
instance Typeable Char   where typeRep = prim "Char"
instance Typeable Int    where typeRep = prim "Int"
instance Typeable Integer where typeRep = nullary "Data.Integer_Type" "Integer"
instance Typeable Double where typeRep = prim "Double"
instance Typeable Word   where typeRep = prim "Word"

instance Typeable TypeRep where typeRep = nullary "Data.Typeable" "TypeRep"
instance Typeable TyCon  where typeRep = nullary "Data.Typeable" "TyCon"

instance forall a . Typeable a => Typeable (IO a) where
  typeRep _ = mkTyConApp (mkTyCon "Primitives" "IO") [typeRep (Proxy :: Proxy a)]
instance forall a . Typeable a => Typeable (Ptr a) where
  typeRep _ = mkTyConApp (mkTyCon "Primitives" "Ptr") [typeRep (Proxy :: Proxy a)]
instance forall a . Typeable a => Typeable (IOArray a) where
  typeRep _ = mkTyConApp (mkTyCon "Primitives" "IOArray") [typeRep (Proxy :: Proxy a)]

instance forall a b . (Typeable a, Typeable b) => Typeable (Either a b) where
  typeRep _ = mkTyConApp (mkTyCon "Data.Either" "Either") [typeRep (Proxy :: Proxy a), typeRep (Proxy :: Proxy b)]
instance forall a . Typeable a => Typeable [a] where
  typeRep _ = mkTyConApp (mkTyCon "Data.List_Type" "[]") [typeRep (Proxy :: Proxy a)]
instance forall a . Typeable a => Typeable (Maybe a) where
  typeRep _ = mkTyConApp (mkTyCon "Data.Maybe_Type" "Maybe") [typeRep (Proxy :: Proxy a)]
instance forall a . Typeable a => Typeable (Proxy a) where
  typeRep _ = mkTyConApp (mkTyCon "Data.Proxy" "Proxy") [typeRep (Proxy :: Proxy a)]
instance forall a b . (Typeable a, Typeable b) => Typeable (a, b) where
  typeRep _ = mkTyConApp (mkTyCon "Data.Tuple" ",") [typeRep (Proxy :: Proxy a), typeRep (Proxy :: Proxy b)]

