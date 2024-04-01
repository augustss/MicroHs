module System.IO.TimeMilli(getTimeMilli) where

foreign import ccall "GETTIMEMILLI" c_getTimeMilli :: IO Int

getTimeMilli :: IO Int
getTimeMilli = c_getTimeMilli
