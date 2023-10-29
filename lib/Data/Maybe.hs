-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Maybe(module Data.Maybe) where
import Primitives
import Data.Bool
import Data.Eq

data Maybe a = Nothing | Just a

instance forall a . Eq a => Eq (Maybe a) where
  Nothing == Nothing  =  True
  Just x  == Just x'  =  x == x'
  _       == _        =  False

maybe :: forall a r . r -> (a -> r) -> Maybe a -> r
maybe r _ Nothing = r
maybe _ f (Just a) = f a

fromMaybe :: forall a . a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

catMaybes :: forall a . [Maybe a] -> [a]
catMaybes mxs = [ x | Just x <- mxs ]

isJust :: forall a . Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

{-
mapMaybe is in Data.List to avoid recursive modules
maybeToList is in Data.List to avoid recursive modules
-}
