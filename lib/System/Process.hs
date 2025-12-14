module System.Process(
  callCommand,
  system,
  readProcess,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Exception(bracket_)
import Control.Monad(when)
import Foreign.C.String
import System.Directory
import System.Exit
import System.IO.Base

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

-- An approximation of readProcess
readProcess :: FilePath -> [String] -> String -> IO String
readProcess cmd args sin = do
  tmpDir <- getTemporaryDirectory
  (fin, hin) <- openTempFile tmpDir "in.txt"
  hPutStr hin sin
  hClose hin
  (fout, hout) <- openTempFile tmpDir "out.txt"
  bracket_ (return ())
           (removeFile fin >> removeFile fout)
           (do
               callCommand $ unwords $ cmd : ("<" ++ fin) : (">" ++ fout) : args
               res <- hGetContents hout
               length res `seq` return res)
