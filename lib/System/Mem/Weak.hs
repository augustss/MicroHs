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
import Primitives(IO, primReturn, primThen)
import Data.Maybe_Type

-- Fake weak references, for now.
-- Correct API, but does not GC properly.

data Weak v = Weak v (Maybe (IO ()))

mkWeak :: k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeak k v fin = primReturn (Weak v fin)

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak (Weak v _) = primReturn (Just v)

finalize :: Weak v -> IO ()
finalize (Weak _ Nothing)    = primReturn ()
finalize (Weak _ (Just fin)) = fin

mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)
mkWeakPtr k fin = mkWeak k k fin

addFinalizer :: key -> IO () -> IO ()
addFinalizer k fin = mkWeakPtr k (Just fin)`primThen` primReturn ()

mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k, v))
mkWeakPair k v fin = mkWeak k (k, v) fin
