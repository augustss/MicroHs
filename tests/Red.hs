module Red where

-- Test reduction K2 x y = K x
k2 :: a -> b -> c -> a
k2 x y z = x

--f2 :: a      -> c -> a
f2 x = k2 x ()

main :: IO ()
main = do
  cprint k2
--  print (f2 () ())
  cprint f2

-- Z K x y z = K x z = x

-- C K2 I x y z = K2 (I x) y z = I x = x
