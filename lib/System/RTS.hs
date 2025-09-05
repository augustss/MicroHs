module System.RTS(gc, Stats(..), getStats) where
import qualified Prelude()
import MiniPrelude
import Primitives

gc :: IO ()
gc = primGC 0

data Stats = Stats
  { cellsAllocated :: Word
  , reductions     :: Word
  } deriving (Show)

getStats :: IO Stats
getStats = do
  (a, r) <- primStats
  return Stats{ cellsAllocated = a, reductions = r }
