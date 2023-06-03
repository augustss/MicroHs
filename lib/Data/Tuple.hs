module Data.Tuple(module Data.Tuple) where

--data (a,b) = (a,b)  -- all tuples are built in

fst :: (a, b) -> a
fst p =
  case p of
    (a, _) -> a

snd :: (a, b) -> b
snd p =
  case p of
    (_, b) -> b

data Unit = Unit   -- Parser hacks allows () to be used

