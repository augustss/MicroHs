-- | A GC-managed reference to a JavaScript value (cf. GHC's wasm JSVal).
--
-- A 'JSVal' owns a handle into the runtime's JS object registry
-- (@globalThis.__mhs.obj@): a JS body allocates one with @__mhs.intern(v)@ and
-- returns it, 'newJSVal' attaches a finalizer that frees the slot when the
-- 'JSVal' is collected, so the JS value is released instead of leaked.
-- 'JSVal's are not serializable.
module JSVal(JSVal, newJSVal, withJSVal) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Primitives(primIntToPtr, primPtrToInt)

-- The runtime FinalizerPtr that frees a JS object-handle slot.
foreign import ccall "&mhs_js_obj_free" jsObjFinalizer :: FinalizerPtr ()

newtype JSVal = JSVal (ForeignPtr ())

-- | Take ownership of a fresh handle from @__mhs.intern@ (which is always >= 1).
-- Like 'newForeignPtr', a handle must be wrapped at most once.
newJSVal :: Int -> IO JSVal
newJSVal h
  | h < 1     = error "newJSVal: invalid handle"
  | otherwise = do
      fp <- newForeignPtr jsObjFinalizer (primIntToPtr h)
      return (JSVal fp)

-- | Run an action with the raw handle, keeping the 'JSVal' alive across it.
-- The handle is valid only inside the callback: it must not be retained after
-- the callback returns or after the 'JSVal' is dropped.
withJSVal :: JSVal -> (Int -> IO a) -> IO a
withJSVal (JSVal fp) k = withForeignPtr fp (\ p -> k (primPtrToInt p))
