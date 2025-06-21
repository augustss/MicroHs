module TypeApp where
import Data.Typeable

foo :: forall a b . (a, b)
foo = (undefined, undefined)

xread :: forall a -> Read a => String -> a
xread t s = read s :: t

incr :: forall a -> Num a => a -> a
incr _ x = x + 1

main :: IO ()
main = do
  print $ read @Int "123"
  print $ xread Int "456"
  let (x, y) = foo @_ @Bool
  print $ typeOf y
  print (incr Int 41)
  return @IO ()
