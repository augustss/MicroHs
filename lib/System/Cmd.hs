module System.Cmd(system) where
import Prelude(); import MiniPrelude
import Foreign.C.String
import System.Exit

foreign import ccall "system" c_system :: CString -> IO Int

system :: String -> IO ExitCode
system s = do
  r <- withCAString s c_system
  return $ if r == 0 then ExitSuccess else ExitFailure r
