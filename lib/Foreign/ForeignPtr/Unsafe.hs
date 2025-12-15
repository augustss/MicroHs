module Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr) where

import qualified Prelude ()
import Primitives

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr = primForeignPtrToPtr
