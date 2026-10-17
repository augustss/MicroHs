module System.IO.TimeMilli(getTimeMicro, getTimeMilli, getBootTimeMicro) where
import qualified Prelude(); import MiniPrelude
import Data.Word(Word)

foreign import ccall "GETTIMEMICRO" c_getTimeMicro :: IO Word
foreign import ccall "GETBOOTTIMEMICRO" c_getBootTimeMicro :: IO Word

getTimeMicro :: IO Word
getTimeMicro = c_getTimeMicro

getTimeMilli :: IO Word
getTimeMilli = do t <- c_getTimeMicro; return (t `quot` 1000)

getBootTimeMicro :: IO Word
getBootTimeMicro = c_getBootTimeMicro
