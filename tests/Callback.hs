module Callback where

foreign export ccall "hsexp" cb :: Int -> IO Int
foreign import ccall "hsimp.h hsimp" imp :: Int -> IO Int

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

--pp :: Int -> IO ()
--pp = _primitive "IO.pp"

cb :: Int -> IO Int
cb n = do
  let r = fac n
  putStrLn $ "cb: " ++ show r
  return r

doit :: Int -> IO ()
doit n = do
  r <- imp n
  putStrLn $ "doit: " ++ show r

main :: IO ()
main = do
  mapM_ doit [1..5]
