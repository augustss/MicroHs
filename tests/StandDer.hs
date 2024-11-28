module StandDer where

data X = X
deriving instance Show X

data T a = A | B | C a | D (T a)

deriving instance (Show a) => Show (T a)

main :: IO ()
main = do
  print X
  print [A, B, C True, D A]
