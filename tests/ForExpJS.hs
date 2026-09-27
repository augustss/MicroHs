module ForExpJS where

foreign export javascript addOne :: Int -> IO Int
foreign export javascript "isPos" gt0 :: Int -> Bool
foreign export javascript scale :: Double -> Double -> Double

addOne :: Int -> IO Int
addOne x = do
  putStrLn "addOne called"
  pure (x + 1)

gt0 :: Int -> Bool
gt0 x = x > 0

scale :: Double -> Double -> Double
scale = (*)
