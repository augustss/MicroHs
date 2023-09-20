module Unsafe.Coerce(module Unsafe.Coerce, Any, primIsInt, primIsIO) where
import Primitives

unsafeCoerce :: forall a b . a -> b
unsafeCoerce = primUnsafeCoerce
