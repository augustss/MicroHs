module System.IO.StringHandle(handleWriteToString, stringToHandle) where
import System.IO

handleWriteToString :: (Handle -> IO ()) -> IO String
handleWriteToString = error "ghc: no handleWriteToString"

stringToHandle :: String -> IO Handle
stringToHandle = error "ghc: stringToHandle"
