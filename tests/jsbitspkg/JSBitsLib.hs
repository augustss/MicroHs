module JSBitsLib(jsBitsAdd) where

-- jsBitsLibAdd is defined in the jsbits of the JSBits package
foreign import javascript "return jsBitsLibAdd($0, $1)" jsBitsAdd :: Int -> Int -> IO Int
