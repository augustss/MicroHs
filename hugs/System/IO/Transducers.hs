module System.IO.Transducers(addLZ77) where
import System.IO

addLZ77 :: Handle -> IO Handle
addLZ77 _ = error "ghc compiled, cannot compress"
