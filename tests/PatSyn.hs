module PatSyn where
import PatSynE

f1 :: [a] -> (a, [a])
f1 (Sings a as) = (a, as)

f2 :: [a] -> a
f2 (Sing a) = a

main :: IO ()
main = do
  print (Sing 1)
  print (Swap 1 2)
  print (f1 [3])
  print (f2 [4])
--  print (Sings 1 [2])  this is an error, of course
