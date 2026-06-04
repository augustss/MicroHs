module System.IO.TimeMicro(getTimeMicro) where
import Data.Time
import Data.Time.Clock.POSIX

getTimeMicro :: IO Int
getTimeMicro = floor . (1000000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime
