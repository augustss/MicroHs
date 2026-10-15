{-# LANGUAGE DuplicateRecordFields #-}
module RecordExportDefs (First(..), Last(Last, other), Box(..)) where

-- The exported owner occurs both before and after a private duplicate.
data First = First { shared :: String }
newtype Box = Box { shared :: Bool }
data Private = Private { shared :: Maybe String }
data Hidden = Hidden { other :: Maybe String }
data Last = Last { other :: String }
