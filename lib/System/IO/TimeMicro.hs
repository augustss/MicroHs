module System.IO.TimeMicro(getTimeMicro) where
import qualified Prelude(); import MiniPrelude

foreign import ccall "GETTIMEMICRO" c_getTimeMicro :: IO Int

getTimeMicro :: IO Int
getTimeMicro = c_getTimeMicro
