-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Maybe(module Data.Maybe) where

data Maybe a = Nothing | Just a

maybe :: r -> (a -> r) -> Maybe a -> r
maybe r f arg =
  case arg of
    Nothing -> r
    Just a  -> f a

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe f am =
  case am of
    Nothing -> Nothing
    Just a  -> Just (f a)
