module Via where

newtype B = B Bool
  deriving newtype Eq

newtype N = N Int
  deriving newtype (Eq, Ord)
  deriving stock (Show)
  deriving newtype Num

data Id a = Id a
  deriving stock Eq

newtype BI = BI (Id Int)
  deriving newtype Eq

newtype Id2 a = Id2 (Id a)
  deriving newtype Eq

------

import Numeric

newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode = U Int
  deriving Show via (Hex Int)

-- >>> euroSign
-- 0x20ac
euroSign :: Unicode
euroSign = U 0x20ac

-------

class SPretty a where
  sPpr :: a -> String
  default sPpr :: Show a => a -> String
  sPpr = show

data S = S String
  deriving stock Show
  deriving anyclass SPretty

-------

main :: IO ()
main = do
  print $ B True == B True

  let x = N 1
      y = N 2
  print $ x == x
  print $ x == y
  print $ x < y
  print $ x + y

  print $ BI (Id 1) == BI (Id 1)

  print $ Id2 (Id False) == Id2 (Id False)

  print euroSign

  print $ sPpr $ S "hello"

  putStrLn "done"
