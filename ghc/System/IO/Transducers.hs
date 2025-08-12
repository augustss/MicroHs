module System.IO.Transducers(addLZ77, addBase64) where
import System.IO

addLZ77 :: Handle -> IO Handle
addLZ77 _ = error "ghc compiled, cannot compress"

addBase64 :: Handle -> IO Handle
addBase64 _ = error "ghc compiled, cannot base64"
