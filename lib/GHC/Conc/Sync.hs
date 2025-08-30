-- GHC compatibility
module GHC.Conc.Sync(STM, TVar, newTVar, newTVarIO, readTVar, writeTVar) where
import Control.Concurrent.STM
