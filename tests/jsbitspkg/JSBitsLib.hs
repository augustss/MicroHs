module JSBitsLib(jsBitsAdd) where

-- jsBitsLibAdd is defined in the jsbits of the JSBits package
foreign import javascript "return jsBitsLibAdd($1, $2)" jsBitsAdd :: Int -> Int -> IO Int
