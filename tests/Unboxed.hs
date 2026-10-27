-- Test fake unboxed tuples and sums
module Unboxed where
import Data.Typeable

f# :: Int -> (# Int, Int #)
f# x = (# x , x+1 #)

type T# = (# Int | Bool #)

g :: Int -> T#
g x = (# x | #)

h :: Bool -> Unboxed.T#
h x = (# | x #)

d :: (# Int | Bool #) -> String
d (# x | #) = show x
d (# | x #) = show x

-- (##) is the empty unboxed tuple (unordered-containers uses it)
u :: Bool -> (# (##) | Int #)
u True = (# (##) | #)
u False = (# | 1 #)

du :: (# (##) | Int #) -> String
du (# (##) | #) = "unit"
du (# | n #) = show n

-- An operator starting with # is still an operator (miso has (#>))
(#>) :: Int -> Int -> Int
x #> y = x * 10 + y

-- Test that regular # still work
(#) :: Int -> Int -> Int
x # y = 2*x - y

main :: IO ()
main = do
  case Unboxed.f# 2 of
    (# x, y #) -> print (x, y)
  putStrLn $ d $ g 2
  putStrLn $ d $ h True
  putStrLn $ du $ u True
  putStrLn $ du $ u False
  print ((#>) 1 2, 3 #> 4, (#> 5) 6)
  print ('a', 'b'#)
  print "abc"#
  print $ typeOf "ab"#
  print $ typeOf "ab"
  print $ 3 # 2
  print $ (#) 3 2
  print $ (3 #) 2
  print $ (# 2) 3
