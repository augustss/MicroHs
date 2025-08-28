module Data.Proxy(module Data.Proxy) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Bool_Type
import Data.Eq
import Data.Functor
import {-# SOURCE #-} Data.Typeable
import Text.Show

type Proxy :: forall (k::Kind) . k -> Type
data Proxy a = Proxy

instance Show (Proxy a) where
  show _ = "Proxy"

instance Eq (Proxy a) where
  _ == _  =  True

instance Functor Proxy where
  fmap _ Proxy = Proxy
