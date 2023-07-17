module Debug.Trace(module Debug.Trace) where
import Prelude
import Primitives

trace :: String -> a -> a
trace msg a = primitive "IO.performIO" (
  do
    putStrLn msg
    return a
  )
