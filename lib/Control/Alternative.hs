module Control.Alternative(module Control.Alternative) where
import Primitives
import Control.Applicative
import Data.Bool_Type
import Data.Functor
import Data.List

infixl 3 <|>

class Applicative f => Alternative (f :: Type -> Type) where
    empty :: forall a . f a
    (<|>) :: forall a . f a -> f a -> f a

    some :: forall a . f a -> f [a]
    some a = (:) <$> a <*> many a

    many :: forall a . f a -> f [a]
    many a = some a <|> pure []

guard :: forall (f :: Type -> Type) a . Alternative f => Bool -> f ()
guard b = if b then pure () else empty
