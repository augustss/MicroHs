module Data.Tuple(module Data.Tuple) where
import Data.Bool

data Unit = Unit   -- Parser hacks allows () to be used

--data (a,b) = (a,b)  -- all tuples are built in

fst :: (a, b) -> a
fst p =
  case p of
    (a, _) -> a

snd :: (a, b) -> b
snd p =
  case p of
    (_, b) -> b

pair :: a -> b -> (a, b)
pair x y = (x, y)

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb ab1 ab2 =
  case ab1 of
    (a1, b1) ->
      case ab2 of
        (a2, b2) ->
          eqa a1 a2 && eqb b1 b2

