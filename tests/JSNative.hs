module JSNative(main) where
import Control.Monad (void)
import System.Environment
import GHC.Wasm.Prim
-- Uses the JavaScript FFI, but is compiled for a native target: the JavaScript
-- code is left out, and nothing JavaScript-specific may reach the C compiler.
main :: IO ()
main = do
  args <- getArgs
  if args == ["js"]
    then void (syncCallback (putStrLn "never called"))
    else putStrLn "native build with JavaScript callbacks: ok"
