module Control.Alternative(module Control.Alternative) where
import Primitives
import Control.Applicative
import Data.Bool_Type
import Data.Functor
import Data.List_Type

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

asum :: forall (f :: Type -> Type) a . Alternative f => [f a] -> f a
asum [] = empty
asum (a:as) = a <|> asum as
