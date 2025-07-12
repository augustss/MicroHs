module Foreign.ForeignPtr_Type (
  ForeignPtr,
  FinalizerPtr,
) where

import qualified Prelude ()
import Primitives

type FinalizerPtr a = FunPtr (Ptr a -> IO ())
