module Data.Text.IO(readFile, writeFile) where
import Prelude hiding (readFile, writeFile)
import qualified Prelude as P
import Data.Text

readFile :: FilePath -> IO Text
readFile fn = pack <$> P.readFile fn

writeFile :: FilePath -> Text -> IO ()
writeFile fn t = P.writeFile fn (unpack t)
