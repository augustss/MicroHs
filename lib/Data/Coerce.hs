module Data.Coerce(Coercible, coerce) where
import qualified Prelude()
import Primitives
import {-# SOURCE #-} Data.Typeable

type Coercible :: forall k . k -> k -> Constraint
class Coercible a b

coerce :: forall a b . Coercible a b => a -> b
coerce = primUnsafeCoerce
