module Debug.Trace(module Debug.Trace) where
import qualified Prelude(); import MiniPrelude
import System.IO.Base(hPutStrLn, stderr, hFlush)
import Primitives

trace :: forall a . String -> a -> a
trace msg a = _primitive "IO.performIO" (
  do
    hPutStrLn stderr msg
    hFlush stderr
    return a
  )

traceM :: forall m . Monad m => String -> m ()
traceM s = trace s (return ())
