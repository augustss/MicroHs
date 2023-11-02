-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Maybe(module Data.Maybe, module Data.Maybe_Type) where
import Primitives
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.Int
import Data.List
import Data.Maybe_Type
import Data.Ord
import Text.Show


instance forall a . Eq a => Eq (Maybe a) where
  Nothing == Nothing  =  True
  Just x  == Just x'  =  x == x'
  _       == _        =  False

instance forall a . (Show a) => Show (Maybe a) where
  showsPrec _ Nothing  = showsPrec 0 "Nothing"
  showsPrec p (Just a) = showParen (p >= 11) (showString "Just " . showsPrec 11 a)

-- XXX instance Monad Maybe

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

mapMaybe :: forall a b . (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (a:as) =
  case f a of
    Nothing -> mapMaybe f as
    Just b -> b : mapMaybe f as

maybeToList :: forall a . Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

