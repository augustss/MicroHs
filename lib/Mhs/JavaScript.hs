-- | A GC-managed reference to a JavaScript value, modelled on GHC's wasm @JSVal@.
--
-- A 'JSVal' is a handle into the runtime's JS object registry
-- (@globalThis.__mhs.obj@), wrapped in a 'ForeignPtr' whose finalizer frees the
-- slot when the 'JSVal' is collected, so the JS value is released rather than
-- leaked.  A @foreign import javascript@ takes and returns 'JSVal' directly: the
-- runtime interns the value and attaches the finalizer, so the type is opaque
-- and there is no raw handle to mismanage.  'JSVal's are not serializable.
module Mhs.JavaScript(JSVal) where
import qualified Prelude(); import MiniPrelude
import Foreign.ForeignPtr(ForeignPtr)

-- The phantom argument to the 'ForeignPtr' distinguishes a JS-object handle from
-- an ordinary foreign pointer.  MicroHs marshals @ForeignPtr JSValRep@ as a
-- JavaScript value (tag 'J'); the name must agree with jsScalarTag in ExpPrint.
data JSValRep

-- | A garbage-collected reference to a JavaScript value.
newtype JSVal = JSVal (ForeignPtr JSValRep)
