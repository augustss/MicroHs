-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Control.Exception(
  catch, try,
  throwIO,
  Exn(..),
  exnToString,
  onException,
  SomeException,
  ) where
import Primitives
import Control.Exn

type SomeException = Exn

catch :: forall a . IO a -> (Exn -> IO a) -> IO a
catch ioa hdl = primCatch ioa (hdl . Exn)

try :: forall a . IO a -> IO (Either Exn a)
try ioa = catch (fmap Right ioa) (return . Left)

throwIO :: forall a . Exn -> IO a
throwIO (Exn s) =
  let e = error s
  in  seq e (return e)

onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do _ <- what
                                          throwIO e
