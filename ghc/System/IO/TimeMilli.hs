module System.IO.TimeMilli(getTimeMilli, getTimeMicro, getBootTimeMicro) where
import Data.Time
import Data.Time.Clock.POSIX

getTimeMilli :: IO Word
getTimeMilli  = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

getTimeMicro :: IO Word
getTimeMicro  = floor . (1000000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

getBootTimeMicro :: IO Word
getBootTimeMicro = return 0
