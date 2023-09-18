-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Tuple(module Data.Tuple
--Y{-
                 , ()(..)
--Y-}
                 ) where
import Primitives  -- for ()
import Data.Bool

--data (a,b) = (a,b)  -- all tuples are built in
--data (a,b,c) = (a,b,c)
-- etc

fst :: forall a b . (a, b) -> a
fst (a, _) = a

snd :: forall a b . (a, b) -> b
snd (_, b) = b

pair :: forall a b . a -> b -> (a, b)
pair x y = (x, y)

eqPair :: forall a b . (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a1, b1) (a2, b2) = eqa a1 a2 && eqb b1 b2
