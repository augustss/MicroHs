module Unsafe.Coerce(module Unsafe.Coerce, AnyType) where
import Prelude()              -- do not import Prelude
import Primitives

unsafeCoerce :: forall a b . a -> b
unsafeCoerce = primUnsafeCoerce
