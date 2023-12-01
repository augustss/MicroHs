module Data.TypeLits(
  Symbol,
  Nat,
  KnownNat(..),
  KnownSymbol(..),
  ) where
import Primitives
import Prelude

class KnownNat (n :: Nat) where
  natVal :: forall (proxy :: Nat -> Type) . proxy n -> Integer

class KnownSymbol (s :: Symbol) where
  symbolVal :: forall (proxy :: Symbol -> Type) . proxy s -> String
