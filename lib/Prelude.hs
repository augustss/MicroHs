module Prelude(module Prelude) where

--------------------------------

(+)  = primitive "+"
(-)  = primitive "-"
(*)  = primitive "*"
quot = primitive "quot"
rem  = primitive "rem"

subtract = primitive "subtract"
negate x = 0 - x

--------------------------------

(==) = primitive "=="
(/=) = primitive "/="

(<)  = primitive "<"
(<=) = primitive "<="
(>)  = primitive ">"
(>=) = primitive ">="

--------------------------------

chr  = primitive "chr"
ord  = primitive "ord"

--------------------------------

error = primitive "error"

-- (":",     Comb "O")

--------------------------------

data Bool = False | True

(||) :: Bool -> Bool -> Bool
(||) x y =
  case x of
    False -> y
    True  -> True

(&&) :: Bool -> Bool -> Bool
(&&) x y =
  case x of
    False -> False
    True  -> y

--------------------------------

--data (a,b) = (a,b)  -- all tuples are built in

fst :: (a, b) -> a
fst p =
  case p of
    (a, _) -> a

snd :: (a, b) -> b
snd p =
  case p of
    (_, b) -> b

data Unit = Unit   -- Parser hacks allows () to be used

--------------------------------

data Maybe a = Nothing | Just a

maybe :: r -> (a -> r) -> Maybe a -> r
maybe r f arg =
  case arg of
    Nothing -> r
    Just a  -> f a

--------------------------------

data Either a b = Left a | Right b

either :: (a -> r) -> (b -> r) -> Either a b -> r
either fa fb arg =
  case arg of
    Left  a -> fa a
    Right b -> fb b

--------------------------------

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

fix = primitive "Y"
