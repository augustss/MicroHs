module System.IO.TimeMilli(getTimeMilli) where
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

getTimeMilli :: IO Word
getTimeMilli  = fmap (floor . (1000 *) . toRational . utcTimeToPOSIXSeconds) getCurrentTime
