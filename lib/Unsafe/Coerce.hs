module Unsafe.Coerce(module Unsafe.Coerce) where
import Primitives

unsafeCoerce :: a -> b
unsafeCoerce = primUnsafeCoerce
