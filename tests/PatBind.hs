module PatBind where

Just x = Just ()

y,z :: Int
(y,z) = (20,30)

u :: Int
(u, v) = (40, True)

a@b = 55 :: Int

main :: IO ()
main = do
  print x
  print y
  print z
  print u
  print v
  print a
  print b
