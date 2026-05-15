module DerivingBuiltin (main) where

import Prelude (Bounded, Enum, Eq, Foldable, Functor, Ord, Int, Read, Show, Traversable)
import Data.Data (Data)
import Data.Ix (Ix)
import Data.Typeable (Typeable)

data T a b c = A a | B b | C a Int | D
    deriving (Bounded, Data, Eq, Ord, Read, Show, Typeable)

data Rec a = R { x :: a, y :: Int }
  deriving (Bounded, Data, Eq, Ord, Read, Show, Typeable)

data E = X | Y | Z
    deriving (Bounded, Data, Enum, Eq, Ix, Ord, Read, Show, Typeable)

data U a = U1 | U2 Int | U3 a | U4 a Int (a, a) | U5 [U a]
    deriving (Functor, Foldable, Traversable)

data V a = V a a a
    deriving (Bounded, Data, Eq, Ix, Ord, Read, Show, Typeable)

data These1 f g a
    = This1 (f a)
    | That1 (g a)
    | These1 (f a) (g a)
  deriving (Functor, Foldable, Traversable, Typeable, Data)

main = main
