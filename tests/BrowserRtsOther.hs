module BrowserRtsOther(main) where
-- Run by the browser runtime built for JSVal (see testbrowserrts): a program
-- other than the one compiled into the runtime, with no foreign imports.
main :: IO ()
main = putStrLn ("another program: " ++ show (sum [1 .. 10 :: Int]))
