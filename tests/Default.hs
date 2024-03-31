module Default(main) where
import Prelude
default (Int, Double, String, ())

main :: IO ()
main = do
  print 1
  print 1.5
  print []
  print "foo"
