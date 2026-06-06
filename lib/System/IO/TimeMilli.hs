module System.IO.TimeMilli(getTimeMicro, getTimeMilli) where
import qualified Prelude(); import MiniPrelude

foreign import ccall "GETTIMEMICRO" c_getTimeMicro :: IO Int

getTimeMicro :: IO Int
getTimeMicro = c_getTimeMicro

getTimeMilli :: IO Int
getTimeMilli = do t <- c_getTimeMicro; return (t `quot` 1000)
