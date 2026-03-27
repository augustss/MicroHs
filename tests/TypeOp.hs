module TypeOp (x, type (~)(), type ($%), default (:%), default ($%)) where
import Data.Coerce
-- import and export types (with class syntax)
import Primitives (type (~)(..))

data a + b = Plus a b
type Foo = Int + Bool

x :: Int `Either` Bool
x = Left 5

type a ~~ b = Coercible a b

class a ~~ b => TypeOp a b

instance a ~~ b => TypeOp a b

f :: a ~~ b => a -> b
f = coerce

main :: IO ()
main = print (f x :: Either Int Bool)

-- Check that various combinations parse.
type (:$$) = Maybe
type (:+) a = Maybe a
type (a :$) = a
type a :++ b = Either a b
type (a :+++ b) c = (a,b,c)

type ($$$) = Maybe
type ($+) a = Maybe a
type (a $$) = a
type a $++ b = Either a b
type (a $+++ b) c = (a,b,c)

data (:$%$)
data (:$$%) a
data (a :$$$%)
data a :$%% b
data (a :$%%% b) c

class (:%$) where
class (:%) a where
class (a :$%) where
class a :%% b where
class (a :%%% b) c where

class (%$%) where
class ($%) a where
class (a $$%) where
class a $%% b where
class (a $%%% b) c where

x1 :: ($%) a => a
x1 = undefined
x2 :: ($%%) a b => a
x2 = undefined
x3 :: a $%% b => a
x3 = undefined
x4 :: ($%%%) a b c => a
x4 = undefined
x5 :: (a $%%% b) c => a
x5 = undefined
x6 :: (a $$%) => a
x6 = undefined

instance ($%) Int
instance (Int $$%)
instance Int $%% Int
instance ($%%) Bool Bool
instance (Int $%%% Int) Int

default (:%) (Int)
default ($%) (Int)
default ((:$%$))
default ($$%) ((:$%$))
