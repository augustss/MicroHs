module RealFloat where

printInfo :: forall a . (RealFloat a, Show a) => a -> IO ()
printInfo x = do
  print (floatRadix x, floatDigits x, floatRange x)
  print (decodeFloat x)
  print (encodeFloat 10 (-2))
  print (exponent x)
  print (significand x)
  print (scaleFloat 4 x)
  print (isNaN x, isNaN (0 / (0 :: a)))
  print (isInfinite x, isInfinite (x / 0))
  print (isDenormalized x)
  print (isNegativeZero x, isNegativeZero (-0 :: a))
  print (isIEEE x)

main :: IO ()
main = do
  printInfo (1.25 :: Double)
  printInfo (1.25 :: Float)
  print (map realToFrac [1.5 :: Double, 1/0, (-1)/0, -0, 0/0] :: [Float])
  print (map realToFrac [1.5 :: Float,  1/0, (-1)/0, -0, 0/0] :: [Double])
  
