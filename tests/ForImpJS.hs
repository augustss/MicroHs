module ForImpJS where
import Foreign.C.String

foreign import javascript "console.log('log: ' + Module.UTF8ToString($1))" clog :: CString -> IO ()
foreign import javascript "return Module.stringToNewUTF8('PRE' + Module.UTF8ToString($1))" pre :: CString -> IO CString
foreign import javascript "return $1 + $2"                           add :: Int -> Int -> Int
foreign import javascript "$1 * $2"                                  mul :: Double -> Double -> Double

hlog :: String -> IO ()
hlog t = withCString t clog

main :: IO ()
main = do
  hlog "JS log"
  hlog "JS log again"
  hlog $ show $ add 3 4
  hlog $ show $ mul 3 4
  s <- withCString "-test" $ \ p -> pre p >>= peekCString
  putStrLn s
