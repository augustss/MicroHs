module ForImpJS where
import Data.Text hiding(show)
import Foreign.C.String(CString)

foreign import javascript "console.log('log: ' + UTF8ToString($0))" clog :: CString -> IO ()
foreign import javascript "return stringToNewUTF8('PRE' + UTF8ToString($0))" pre :: CString -> IO CString
foreign import javascript "return $0 + $1"                           add :: Int -> Int -> Int
foreign import javascript "return $0 * $1"                           mul :: Double -> Double -> Double

hlog :: Text -> IO ()
hlog t = useAsCString t clog

main :: IO ()
main = do
  hlog "JS log"
  hlog "JS log again"
  hlog $ pack $ show $ add 3 4
  hlog $ pack $ show $ mul 3 4
  s <- useAsCString "-test" $ \ p -> pre p >>= grabCString
  putStrLn (unpack s)
