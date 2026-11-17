module System.IO.Transducers(addLZMA, addBase64) where
import System.IO

addLZMA :: Handle -> IO Handle
addLZMA _ = error "hugs compiled, cannot compress"


addBase64 :: Handle -> IO Handle
addBase64 _ = error "hugs compiled, cannot base64"
