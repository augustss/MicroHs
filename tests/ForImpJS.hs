module ForImpJS where
import Data.Text

foreign import javascript "console.log('log: ' + UTF8ToString($0))" clog :: Text -> IO ()
foreign import javascript "return $1 + $2"                           add :: Int -> Int -> Int

main :: IO ()
main = do
  clog "JS log"
  clog "JS log again"
  clog $ pack $ show $ add 3 4
