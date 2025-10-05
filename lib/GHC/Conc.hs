module GHC.Conc(threadStatus, ensureIOManagerIsRunning) where
import Control.Concurrent

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = return ()
