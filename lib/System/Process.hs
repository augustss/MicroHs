module System.Process(callCommand, system) where
import qualified Prelude(); import MiniPrelude
import Control.Monad(when)
import Foreign.C.String
import System.Exit

foreign import ccall "system" systemc :: CString -> IO Int

callCommand :: String -> IO ()
callCommand cmd = do
  rc <- system cmd
  case rc of
    ExitSuccess -> return ()
    ExitFailure r -> error $ "callCommand: failed " ++ show r ++ ", cmd=\n" ++ show cmd

system :: String -> IO ExitCode
system cmd = do
  r <- withCAString cmd systemc
  if r == 0 then
    return ExitSuccess
   else
    return (ExitFailure r)
