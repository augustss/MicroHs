module RecMdl where
import RecMdlA

h :: Int -> Int
h x = x + 100

f :: Int -> Int
f x = g (x+1)

main :: IO ()
main = do
  print (f 10)
