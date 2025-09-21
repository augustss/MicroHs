module System.Timeout(Timeout, timeout) where
import Control.Concurrent
import Control.Exception
import Data.Unique

newtype Timeout = Timeout Unique
  deriving (Eq)

instance Show Timeout where
  show _ = "<<timeout>>"

instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

timeout :: Int -> IO a -> IO (Maybe a)
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\ e -> if e == ex then Just () else Nothing)
                   (\ _ -> return Nothing)
                   (bracket (forkIOWithUnmask $ \ unmask ->
                                 unmask $ threadDelay n >> throwTo pid ex)
                            (uninterruptibleMask_ . killThread)
                            (const $ fmap Just f))
        
