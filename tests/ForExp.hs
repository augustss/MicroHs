module ForExp where

foreign export ccall funcName :: Int -> IO Int
foreign export ccall "other" ofuncName :: Int -> IO Int
