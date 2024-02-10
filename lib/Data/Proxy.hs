module Data.Proxy(module Data.Proxy) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool_Type
import Data.Eq
import Data.Functor
import Text.Show

type Proxy :: forall (k::Kind) . k -> Type
data Proxy a = Proxy

instance forall a . Show (Proxy a) where
  show _ = "Proxy"

instance forall a . Eq (Proxy a) where
  _ == _  =  True

instance Functor Proxy where
  fmap _ Proxy = Proxy
