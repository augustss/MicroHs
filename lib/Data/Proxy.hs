module Data.Proxy(module Data.Proxy) where
import Prelude

-- NOTE: not polykinded yet
data Proxy a = Proxy

instance forall a . Show (Proxy a) where
  show _ = "Proxy"

instance forall a . Eq (Proxy a) where
  _ == _  =  True
