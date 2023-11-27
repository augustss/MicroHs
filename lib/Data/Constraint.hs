-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Constraint(module Data.Constraint) where
import Primitives  -- for (->)
import Text.Show

-- A very, very minimal version of the constraints package

data Dict (c :: Constraint) = c => Dict

instance forall (c :: Constraint) . Show (Dict c) where
  showsPrec _ Dict = showString "Dict"

withDict :: forall (c :: Constraint) r . Dict c -> (c => r) -> r
withDict Dict r = r
