module System.IO.TimeMilli(getTimeMilli, getTimeMicro, getBootTimeMicro) where
import Data.Time
import Data.Time.Clock.POSIX

getTimeMilli :: IO Int
getTimeMilli  = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

getTimeMicro :: IO Int
getTimeMicro  = floor . (1000000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

getBootTimeMicro :: IO Int
getBootTimeMicro = return 0
