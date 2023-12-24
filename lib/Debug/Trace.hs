module Debug.Trace(module Debug.Trace) where
import Prelude
import Primitives

trace :: forall a . String -> a -> a
trace msg a = primitive "IO.performIO" (
  do
    putStrLn msg
    return a
  )

traceM :: forall m a . Monad m => String -> m ()
traceM s = trace s (return ())
