module Data.List.NonEmpty_Type(module Data.List.NonEmpty_Type) where
import qualified Prelude()
import {-# SOURCE #-} Data.Typeable

infixr 5 :|

data NonEmpty a = a :| [a]
