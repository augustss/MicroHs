module Reflect(main) where
import Mhs.Reflect

data Color = Red | Green | Blue | Cyan | Magenta | Yellow | Black | White

-- A sparse case
isWarm :: Color -> Bool
isWarm Red = True
isWarm Yellow = True
isWarm _ = False

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

main :: IO ()
main = do
  let sh :: a -> IO ()
      sh a = reflect a >>= putStr . ppProg >> putStrLn ""
  sh (swap :: (Int, Bool) -> (Bool, Int))
  sh isWarm
  sh (maybe False not)
  sh (\ xs -> case xs of { [] -> 'n'; (y:_) -> y })
