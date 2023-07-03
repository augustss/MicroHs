module Control.Error(module Control.Error) where
import Primitives

--error :: String -> a
error = primError

undefined :: a
undefined = error "undefined"
