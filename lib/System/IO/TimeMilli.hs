module System.IO.TimeMilli(getTimeMilli) where
import qualified Prelude(); import MiniPrelude

foreign import ccall "GETTIMEMILLI" c_getTimeMilli :: IO Int

getTimeMilli :: IO Int
getTimeMilli = c_getTimeMilli
