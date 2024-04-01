module Control.Exn(Exn, exnToString) where
import Control.Exception

type Exn = SomeException

exnToString :: Exn -> String
exnToString = trunc . show
  where trunc = head . lines

