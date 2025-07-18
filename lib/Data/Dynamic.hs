module Data.Dynamic(
  Typeable(..),
  Dynamic,
  toDyn,
  fromDyn, fromDynamic,
  dynApply, dynApp,
  dynTypeRep,
  ) where
import Prelude
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Unsafe.Coerce
import Control.Exception

data Dynamic = D TypeRep AnyType
  deriving Typeable

instance Exception Dynamic

instance Show Dynamic where
  show (D t _) = "<<" ++ show t ++ ">>"

toDyn :: forall a . Typeable a => a -> Dynamic
toDyn a = D (typeOf a) (unsafeCoerce a)

fromDyn :: forall a . Typeable a => Dynamic -> a -> a
fromDyn d a = fromMaybe a $ fromDynamic d

fromDynamic :: forall a . Typeable a => Dynamic -> Maybe a
fromDynamic (D tr a) | tr == typeRep (Proxy :: Proxy a) = Just (unsafeCoerce a)
                     | otherwise = Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f a = fromMaybe (error "Dynamic.dynApp") $ dynApply f a

dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (D ftr f) (D atr a) = g <$> funResultTy ftr atr
  where g rtr = D rtr (unsafeCoerce f a)

dynTypeRep :: Dynamic -> TypeRep
dynTypeRep (D tr _) = tr
