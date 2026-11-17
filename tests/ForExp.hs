module ForExp where

foreign export ccall funcName :: Int -> IO Int
foreign export ccall "other" (*) :: Int -> Int -> Int
-- A unit result is a constructor, which once crashed a stand-alone export.
foreign export ccall greet :: IO ()

funcName :: Int -> IO Int
funcName x = do
  putStrLn "funcName called"
  pure (x + 1)

greet :: IO ()
greet = putStrLn "greet called"
