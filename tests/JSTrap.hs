-- A wasm trap while main runs (here an out of bounds read) unwinds the runtime
-- without any Haskell code seeing it.  The runtime must be marked as stopped
-- anyway, so that a callback called later is refused instead of running on the
-- wreckage (emscripten target; JSTrap.pre keeps node alive after the trap).
-- Expected: the two lines of JSTrap.ref, exit status 1.
module JSTrap(main) where
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Wasm.Prim

foreign import javascript "setTimeout(() => { try { $1(); console.log('later: the callback ran') } catch (e) { console.log('later: ' + (String(e.message).indexOf('runtime has stopped') >= 0 ? 'refused' : e.message)) } }, 50)"
  later :: JSVal -> IO ()

main :: IO ()
main = do
  cb <- syncCallback (putStrLn "callback ran")
  later cb
  b <- peek (nullPtr `plusPtr` 0x7FFFFFF0 :: Ptr Word8)   -- far outside the wasm memory
  print b
