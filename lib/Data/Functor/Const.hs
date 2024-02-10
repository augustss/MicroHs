-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Functor.Const(Const(..), getConst) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Monoid
import Data.Ord
import Text.Show

type Const :: forall k . Type -> k -> Type
newtype Const a b = Const a
  deriving (Eq, Ord, Show)

getConst :: forall a b . Const a b -> a
getConst (Const a) = a

instance forall a . Functor (Const a) where
  fmap _ (Const a) = Const a

instance forall m . Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a `mappend` b)
