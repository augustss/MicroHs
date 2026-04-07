module TypeApp where
import Data.Typeable

foo :: forall a b . (a, b)
foo = (undefined, undefined)

xread :: forall a -> Read a => String -> a
xread t s = read s :: t

incr :: forall a -> Num a => a -> a
incr _ x = x + 1

incu :: () -> (forall a. Num a => a -> a)
incu _ x = x + 1

inci :: forall a . Num a => a -> forall b . (Num b, Integral b) => b -> a
inci x = (+) x . fromIntegral

sizeOf :: forall a -> Int
sizeOf _ = 0

constT :: forall a b -> a -> b -> a
constT _ _ a b = a

constT2 :: forall a -> a -> forall b -> b -> a
constT2 _ a _ b = a

main :: IO ()
main = do
  print $ read @Int "123"
  print $ xread Int "456"
  let (x, y) = foo @_ @Bool
  print $ typeOf y
  print (incr Int 41)
  print (incu () 41)
--  print (inci 1 41 :: Double)
  print (sizeOf Bool)
  print (constT Int Bool 1 False)
  print (constT2 Int 2 Bool True)
  return @IO ()
