module Unsafe.Coerce(module Unsafe.Coerce, Any) where
import Primitives

unsafeCoerce :: forall a b . a -> b
unsafeCoerce = primUnsafeCoerce
