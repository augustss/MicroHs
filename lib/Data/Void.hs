module Data.Void(module Data.Void) where
import qualified Prelude(); import MiniPrelude
import Text.Read

data Void deriving (Eq, Ord, Read, Show)

absurd :: forall a . Void -> a
absurd v = seq v (error "absurd")

vacuous :: forall f a . Functor f => f Void -> f a
vacuous = fmap absurd
