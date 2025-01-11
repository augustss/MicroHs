module TypeApp where
import Data.Typeable

foo :: forall a b . (a, b)
foo = (undefined, undefined)

main :: IO ()
main = do
  print $ read @Int "123"
  let (x, y) = foo @_ @Bool
  print $ typeOf y
