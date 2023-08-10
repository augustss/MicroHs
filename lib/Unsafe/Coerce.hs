module Unsafe.Coerce(module Unsafe.Coerce) where
import Primitives

unsafeCoerce :: forall a b . a -> b
unsafeCoerce = primUnsafeCoerce
