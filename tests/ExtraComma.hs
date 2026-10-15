module ExtraComma(,x,T(,A,B,),) where

data T = A | B
  deriving (, Eq, Show, )

data S = S1 { , x1 :: Int } | S2 { x2 :: Int , } | S3 { , x3, :: Int }
  deriving (, Show)

x,y, :: Int
x=1
y=2

infix 3 +++,***,
a +++ b = a
a *** b = b

main :: IO ()
main = do
  print [x*x | x <-[1,2], ]
  print (S1 10){ , x1 = 11 }
  print [1,2,3,]
  print [,4,5]
