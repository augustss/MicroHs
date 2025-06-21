module ForExp where

foreign export ccall funcName :: Int -> IO Int
foreign export ccall "other" ofuncName :: Int -> Int -> Int

funcName :: Int -> IO Int
funcName x = pure (x + 1)

ofuncName :: Int -> Int -> Int
ofuncName x y = x * y
