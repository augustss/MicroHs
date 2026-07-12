module System.IO.TimeMilli(getTimeMicro, getTimeMilli, getBootTimeMicro) where
import qualified Prelude(); import MiniPrelude

foreign import ccall "GETTIMEMICRO" c_getTimeMicro :: IO Int
foreign import ccall "GETBOOTTIMEMICRO" c_getBootTimeMicro :: IO Int

getTimeMicro :: IO Int
getTimeMicro = c_getTimeMicro

getTimeMilli :: IO Int
getTimeMilli = do t <- c_getTimeMicro; return (t `quot` 1000)

getBootTimeMicro :: IO Int
getBootTimeMicro = c_getBootTimeMicro
