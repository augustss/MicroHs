module Default(main) where
import Prelude
default (Int, Double)

main :: IO ()
main = do
  print 1
  print 1.5
  print []   -- defaults to Int, a little weird
