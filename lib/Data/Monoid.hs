module Data.Monoid(
  Monoid(..),
  Endo(..),
  Dual(..),
  Sum(..),
  Product(..),
  All(..),
  Any(..),
  Alt(..),
  First(..),
  Last(..),
  Ap(..),
  ) where
import qualified Prelude()
import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Bounded
import Data.Coerce -- needed for deriving
import Data.Enum_Class
import Data.Eq
import Data.Function -- needed for deriving
import Data.Functor
import Data.Maybe_Type
import Data.Monoid.Internal
import Data.Num
import Data.Ord
import Data.Records
import {-# SOURCE #-} Data.Typeable
import Text.Show

-- First and Last are different in Monoid and Semigroup,
-- so put them here.

newtype First a = First { getFirst :: Maybe a }
        deriving ( Eq
                 , Ord
                 , Show
                 , Functor
                 , Applicative
                 , Monad
                 )

instance Semigroup (First a) where
  a@(First (Just _)) <> _ = a
  First Nothing      <> a = a

instance Monoid (First a) where
  mempty = First Nothing


newtype Last a = Last { getLast :: Maybe a }
        deriving ( Eq
                 , Ord
                 , Show
                 , Functor
                 , Applicative
                 , Monad
                 )

instance Semigroup (Last a) where
  _ <> a@(Last (Just _)) = a
  a <>    Last Nothing   = a

instance Monoid (Last a) where
  mempty = Last Nothing

newtype Ap f a = Ap { getAp :: f a }
        deriving ( Alternative
                 , Applicative
                 , Enum
                 , Eq
                 , Functor
                 , Monad
                 , MonadFail
                 , MonadPlus
                 , Ord
                 , Show
                 )

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  Ap x <> Ap y = Ap (liftA2 (<>) x y)

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  mempty = Ap (pure mempty)

instance (Applicative f, Bounded a) => Bounded (Ap f a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (Applicative f, Num a) => Num (Ap f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum
