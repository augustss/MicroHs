-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MHSPrelude(
  module Prelude,
  module Control.Monad.Fail,
  module Data.Monoid,
  (<$>), takeWhileEnd, dropWhileEnd, spanEnd, breakEnd, stripPrefix,
  stripPrefixBy, NonEmpty(..), Semigroup(..), makeVersion) where
import Prelude hiding(fail)
import Control.Applicative
import Control.Monad.Fail
import Data.Monoid
import Data.Version

------- List --------

stripPrefix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripPrefix = stripPrefixBy (==)

stripPrefixBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy eq [] s = Just s
stripPrefixBy eq (c:cs) [] = Nothing
stripPrefixBy eq (c:cs) (d:ds) | eq c d = stripPrefixBy eq cs ds
                               | otherwise = Nothing

takeWhileEnd :: forall a . (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

------- Monoid --------

infixr 5 :|
data NonEmpty a = a :| [a]

infixr 6 <>
class Semigroup a where
  (<>)    :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes  :: (Integral b, Ord b) => b -> a -> a

  sconcat (a :| as) = go a as
    where go b (c:cs) = b <> go c cs
          go b []     = b

  stimes y0 x0
    | y0 <= 0   = error "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | y `rem` 2 == 0 = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (y `quot` 2) x
      g x y z
        | y `rem` 2 == 0 = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (y `quot` 2) (x <> z)

------- Version --------

makeVersion :: [Int] -> Version
makeVersion b = Version b []
