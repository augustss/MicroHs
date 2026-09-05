module Data.TypeLits(
  Symbol,
  Nat,
  KnownNat(..),
  KnownSymbol(..),
  SymbolEq,
  ConcatSymbol,
  HeadSymbol
  ) where
import qualified Prelude()
import Primitives
import Data.Char_Type
import Data.Integer
import {-# SOURCE #-} Data.Typeable

-- Special classes solved by the typechecker.
-- An instance of one of these classes would be useless.

class KnownNat (n :: Nat) where
  natVal :: forall (proxy :: Nat -> Type) . proxy n -> Integer

class KnownSymbol (s :: Symbol) where
  symbolVal :: forall (proxy :: Symbol -> Type) . proxy s -> String

-- Tests two litteral Symbols equality and returns "True" or "False".
class SymbolEq (s :: Symbol) (t :: Symbol) (b :: Symbol) | s t -> b

class ConcatSymbol (s :: Symbol) (t :: Symbol) (st :: Symbol) | s t -> st, st s -> t, st t -> s

class HeadSymbol (h :: Symbol) (t :: Symbol) (s :: Symbol)
  | h t -> s, s -> h t
