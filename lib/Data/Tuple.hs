-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Tuple(module Data.Tuple) where
import Data.Bool

data () = ()   -- Parser hacks allows () to be used --Z

--data (a,b) = (a,b)  -- all tuples are built in
--data (a,b,c) = (a,b,c)
-- etc

fst :: forall a b . (a, b) -> a
fst p =
  case p of
    (a, _) -> a

snd :: forall a b . (a, b) -> b
snd p =
  case p of
    (_, b) -> b

pair :: forall a b . a -> b -> (a, b)
pair x y = (x, y)

eqPair :: forall a b . (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb ab1 ab2 =
  case ab1 of
    (a1, b1) ->
      case ab2 of
        (a2, b2) ->
          eqa a1 a2 && eqb b1 b2

