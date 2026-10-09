module Mhs.Print(cprint, cuprint) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Function
import System.IO
import System.IO.Internal

primHPrint       :: forall a . Ptr BFILE -> a -> IO ()
primHPrint        = _primitive "IO.print"

-- NOTE: This is a dangerous function, it might crash and/or execute effects
cprint :: forall a . a -> IO ()
cprint a = withHandleWr stdout $ \ p ->
  let gc = primGC 1 in
  primRnfNoErr a `seq`    -- this is where the danger lurks
  gc `primThen`           -- Do GC reductions
  gc `primThen`
  gc `primThen`
  gc `primThen`
  gc `primThen`
  gc `primThen`
  primHPrint p a

cuprint :: forall a . a -> IO ()
cuprint a = withHandleWr stdout $ \ p -> primHPrint p a
