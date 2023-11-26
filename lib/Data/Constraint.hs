-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Constraint(module Data.Constraint) where
-- A very, very minimal version of the constraints package

data Dict (c :: Constraint) = c => Dict

withDict :: forall (c :: Constraint) r . Dict c -> (c => r) -> r
withDict Dict r = r
