module MHSPrelude(module Prelude, module Control.DeepSeq, module MHSPrelude, Type) where
import Prelude
import Control.DeepSeq
import Control.Exception
import Data.Maybe
import Data.List
import Data.Text(Text, append, pack)
import GHC.Types
import System.Environment
import System.IO

------- List --------

takeWhileEnd :: forall a . (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

stripSuffix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s t =
  case stripPrefix (reverse s) (reverse t) of
    Nothing -> Nothing
    Just x -> Just (reverse x)

------- IO --------

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM path m = do
  r <- (try $ openFile path m) :: IO (Either IOError Handle)
  case r of
    Left _ -> return Nothing
    Right h -> return (Just h)

openTmpFile :: String -> IO (String, Handle)
openTmpFile tmplt = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "." tmplt

------- Read --------

usingMhs :: Bool
usingMhs = False

_wordSize :: Int
_wordSize = 64

_isWindows :: Bool
_isWindows = False

-- This cannot be implemented with GHC.
rnfNoErr :: forall a . a -> ()
rnfNoErr _ = ()

-- This cannot be implemented with GHC.
rnfErr :: forall a . a -> ()
rnfErr _ = ()

appendDot :: Text -> Text -> Text
appendDot x y = x `append` pack "." `append` y

wantGMP :: Bool
wantGMP = False

compiledWithMhs :: Bool
compiledWithMhs = False
