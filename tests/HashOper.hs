-- Operators that start with '#', like miso's (#) and (#>), next to unboxed tuples.
module HashOper(main) where

infixl 8 #>
(#>) :: Int -> Int -> Int
a #> b = a * 10 + b

infixl 6 #
(#) :: Int -> Int -> Int
a # b = a + b

swap :: (# Int, Int #) -> (# Int, Int #)
swap (# a, b #) = (# b, a #)

unit :: (# #) -> Int
unit (# #) = 42

main :: IO ()
main = do
  print (1 #> 2)
  print ((#>) 3 4)
  print (1 # 2)
  print ((#) 3 4)
  print (map (#> 1) [1, 2])
  print (map (1 #>) [1, 2])
  print (map (# 2) [1, 2])
  let z = 5
  print (map (#z) [1, 2])       -- a right section without a space
  case swap (# 1, 2 #) of
    (# a, b #) -> print (a, b)
  print (unit (##))
  -- unboxed tuples starting with an operator character and no space
  case (#-1, 2 #) of
    (# a, b #) -> print (a + b)
  case (#-3 #) of
    (# a #) -> print a
  case (# ')', "#)" #) of
    (# c, str #) -> putStrLn (c : str)
  -- left sections of the bare # operator, also inside an unboxed tuple
  print (map (1 #) [1, 2])
  case (# (10 #) 1, 2 #) of
    (# a, b #) -> print (a, b)
  -- a tuple over several lines
  case (# 1,
          2 #) of
    (# a, b #) -> print (a * b)
