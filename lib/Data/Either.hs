-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Either(module Data.Either) where

data Either a b = Left a | Right b

either :: forall a b r . (a -> r) -> (b -> r) -> Either a b -> r
either f _ (Left  a) = f a
either _ f (Right b) = f b
