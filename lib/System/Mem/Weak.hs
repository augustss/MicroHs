module System.Mem.Weak(
  Weak,
  mkWeak,
  deRefWeak,
  finalize,
  mkWeakPtr,
  addFinalizer,
  mkWeakPtr,
  ) where
import qualified Prelude()
import Primitives(IO, primReturn, primThen, Weak)
import Data.Maybe_Type

primWeakPtr :: k -> v -> IO (Weak v)
primWeakPtr = _primitive "Wknew"

primWeakPtrFin :: k -> v -> IO () -> IO (Weak v)
primWeakPtrFin = _primitive "Wknewfin"

primDerefWeak :: Weak v -> IO (Maybe v)
primDerefWeak = _primitive "Wkderef"

primFinalizeWeak :: Weak v -> IO ()
primFinalizeWeak = _primitive "Wkfinal"

-- Warning: Do NOT use Int/Word as the key.
-- The garbage collector increases the sharing of small ints.
mkWeak :: k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeak k v Nothing = primWeakPtr k v
mkWeak k v (Just fin) = primWeakPtrFin k v fin

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak = primDerefWeak

finalize :: Weak v -> IO ()
finalize = primFinalizeWeak

mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)
mkWeakPtr k fin = mkWeak k k fin

addFinalizer :: key -> IO () -> IO ()
addFinalizer k fin = mkWeakPtr k (Just fin)`primThen` primReturn ()

mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k, v))
mkWeakPair k v fin = mkWeak k (k, v) fin
