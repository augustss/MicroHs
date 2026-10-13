module RecordExport(main) where

import qualified RecordExportDefs as First (First(..))
import qualified RecordExportDefs as Last (Last(Last, other))
import qualified RecordExportDefs as Box (Box(..))
import qualified RecordExportAgain as AgainFirst (First(First, shared))
import qualified RecordExportAgain as AgainLast (Last(..))
import qualified RecordExportAgain as AgainBox (Box(..))

-- Calling the exported selectors checks both their owner and result type.
-- Record-dot access instead uses HasField and would miss this regression.
main :: IO ()
main = do
  putStrLn (First.shared (First.First "first"))
  putStrLn (Last.other (Last.Last "last"))
  print (Box.shared (Box.Box True))
  putStrLn (AgainFirst.shared (AgainFirst.First "first again"))
  putStrLn (AgainLast.other (AgainLast.Last "last again"))
  print (AgainBox.shared (AgainBox.Box False))
