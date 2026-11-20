module Error112(main) where

-- Only an unsafe (synchronous) JavaScript import can be pure; without a safety
-- annotation an import is asynchronous, as in GHC's WebAssembly backend.
foreign import javascript "$1 + $2" add :: Int -> Int -> Int

main :: IO ()
main = print (add 1 2)
