module Data.Maybe(module Data.Maybe) where

data Maybe a = Nothing | Just a

maybe :: r -> (a -> r) -> Maybe a -> r
maybe r f arg =
  case arg of
    Nothing -> r
    Just a  -> f a
