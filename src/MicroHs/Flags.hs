module MicroHs.Flags(Flags(..), verbosityGT) where
import Prelude

data Flags = Flags {
  verbose    :: Int,        -- verbosity level
  runIt      :: Bool,       -- run instead of compile
  paths      :: [String],   -- module search path
  output     :: String,     -- output file
  loading    :: Bool,       -- show loading message
  readCache  :: Bool,       -- read and use cache
  writeCache :: Bool,       -- generate cache
  useTicks   :: Bool        -- emit ticks
  }
  --deriving (Show)

verbosityGT :: Flags -> Int -> Bool
verbosityGT flags v = flags.verbose > v
