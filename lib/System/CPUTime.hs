module System.CPUTime(getCPUTime, cpuTimePrecision) where
import qualified Prelude()
import MiniPrelude
import Data.Integer
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

foreign import ccall getcpu :: Ptr CULong -> Ptr CULong -> IO ()

getCPUTime :: IO Integer
getCPUTime = do
  psec <- new 0
  pnsec <- new 0
  getcpu psec pnsec
  sec <- peek psec
  nsec <- peek pnsec
  return $ toInteger sec * 1000_000_000_000 + toInteger nsec * 1000

-- Arbitrarily set it to  1us.
-- Using the system call clock_getres() will not give an accurate
-- result anyway.
cpuTimePrecision :: Integer
cpuTimePrecision = 1_000_000
