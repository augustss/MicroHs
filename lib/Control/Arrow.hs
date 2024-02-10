module Control.Arrow(module Control.Arrow) where
import Prelude()              -- do not import Prelude

first :: forall a b c . (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

second :: forall a b c . (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)
