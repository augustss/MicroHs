module ForImpJS where
import Data.FloatW
import Data.Text

foreign import javascript "console.log('log: ' + UTF8ToString($0))" clog :: Text -> IO ()
foreign import javascript "return $0 + $1"                           add :: Int -> Int -> Int
foreign import javascript "return $0 * $1"                           mul :: FloatW -> FloatW -> FloatW
foreign import javascript "return stringToNewUTF8('PRE' + UTF8ToString($0))" pre :: Text -> IO Text

main :: IO ()
main = do
  clog "JS log"
  clog "JS log again"
  clog $ pack $ show $ add 3 4
  clog $ pack $ show $ mul 3 4
  s <- pre (pack "-rest")
  putStrLn (unpack s)
