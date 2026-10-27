module JSFlag where

-- jsFlagAdd is defined in JSFlag.pre, which is embedded as jsflag.js
foreign import javascript "return jsFlagAdd($1, $2)" jsFlagAdd :: Int -> Int -> IO Int

main :: IO ()
main = do
  n <- jsFlagAdd 40 2
  print n
