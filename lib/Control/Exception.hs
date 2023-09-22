module Control.Exception(
  catch, try,
  throwIO,
  Exn(..)
  ) where
import Primitives
import Prelude

newtype Exn = Exn String

catch :: forall a . IO a -> (Exn -> IO a) -> IO a
catch ioa hdl = primCatch ioa (hdl . Exn)

try :: forall a . IO a -> IO (Either Exn a)
try ioa = catch (fmap Right ioa) (return . Left)

throwIO :: forall a . Exn -> IO a
throwIO (Exn s) =
  let e = error s
  in  seq e (return e)
