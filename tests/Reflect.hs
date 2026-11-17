module Reflect(main) where
import Mhs.Reflect
import Primitives(primIntAdd, primIntSub, primIntEQ, primDoubleMul)
import System.Mem(performGCWithReduction)

data Color = Red | Green | Blue | Cyan | Magenta | Yellow | Black | White

-- A sparse case
isWarm :: Color -> Bool
isWarm Red = True
isWarm Yellow = True
isWarm _ = False

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- Local recursion.
-- The examples use primitives rather than class methods,
-- so the output does not depend on how the library is compiled
-- (but see fac).
len :: [a] -> Int
len = go 0
  where go n [] = n
        go n (_:xs) = go (primIntAdd n 1) xs

-- Mutual recursion
evn, odd' :: Int -> Bool
evn n = if primIntEQ n 0 then True else odd' (primIntSub n 1)
odd' n = if primIntEQ n 0 then False else evn (primIntSub n 1)

-- This uses class methods.  Evaluating fac once replaces the method
-- selections from the Num Int dictionary with the primitives.
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

answer :: Bool -> String
answer b = if b then "yes" else "no"

scale :: Double -> Double
scale x = primDoubleMul x 2.5

-- A case that returns a function.  The arity of the False arm is guessed wrong.
pick :: Bool -> Int -> Int
pick b = if b then primIntAdd 1 else id

main :: IO ()
main = do
  let sh :: a -> IO ()
      sh a = do
        -- The GC reduces some combinators (e.g., C op becomes the flipped op),
        -- which changes the output.  A compiled program has been GC reduced
        -- after loading, but not one run with mhs -r.  So do the same reductions
        -- as the loader to get the same result either way.
        performGCWithReduction
        performGCWithReduction
        reflect a >>= putStr . ppProg >> putStrLn ""
  sh (swap :: (Int, Bool) -> (Bool, Int))
  sh isWarm
  sh (maybe False not)
  sh (\ xs -> case xs of { [] -> 'n'; (y:_) -> y })
  sh (len :: [Int] -> Int)
  sh evn
  print (fac 5)
  sh fac
  sh answer
  sh scale
  sh (\ (x :: Int) -> (x, x))
  sh (123456789012345678901234567890 :: Integer)
  sh pick
