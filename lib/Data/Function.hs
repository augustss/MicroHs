module Data.Function(module Data.Function) where
import Primitives

($) :: (a -> b) -> a -> b
($) f x = f x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (b -> a -> c) -> a -> b -> c
flip f a b = f b a

fix = primFix
