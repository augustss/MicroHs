module ForExp where

foreign export ccall funcName :: Int -> IO Int
foreign export ccall "other" (*) :: Int -> Int -> Int

funcName :: Int -> IO Int
funcName x = do
  putStrLn "funcName called"
  pure (x + 1)
