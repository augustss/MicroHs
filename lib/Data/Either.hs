module Data.Either(module Data.Either) where

data Either a b = Left a | Right b

either :: (a -> r) -> (b -> r) -> Either a b -> r
either fa fb arg =
  case arg of
    Left  a -> fa a
    Right b -> fb b

