module System.IO.Transducers(addLZMA, addBase64) where
import System.IO

addLZMA :: Handle -> IO Handle
addLZMA _ = error "ghc compiled, cannot compress"

addBase64 :: Handle -> IO Handle
addBase64 _ = error "ghc compiled, cannot base64"
