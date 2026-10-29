-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
--
-- JavaScript values and callbacks, only available with the emscripten target.
-- The API follows GHC's WebAssembly backend (GHC.Wasm.Prim), with the
-- GHCJS style callback functions (GHC.JS.Foreign.Callback) added.
--
-- A JSVal is a handle to a value in a table on the JavaScript side.
-- The table entry is dropped when the JSVal is garbage collected, or eagerly
-- with freeJSVal.
--
-- JavaScript code in 'foreign import javascript' is compiled into a function
-- with the parameters $1, $2, ...; it can be an expression or statements
-- (with 'return').  The code can also use the emscripten 'Module' object
-- (e.g., Module.UTF8ToString) and 'mhsjs', the JSVal table.
-- Allowed argument and result types are JSVal (and newtypes of it, e.g. JSString),
-- Int, Word, Char, Bool, Double, Float, Ptr, and () as result.
--
-- The import string "wrapper sync" turns a Haskell function of type
-- JSVal -> ... -> IO () (or IO JSVal) into a JavaScript function that calls
-- the Haskell function; "wrapper" does the same but the JavaScript function
-- runs the Haskell code later and returns a Promise.
--
-- With 'unsafe' (the default) a JavaScript exception in the code is fatal.
-- With 'safe' it is raised as a JSException.
-- With 'interruptible' the code is an async function, so it can use 'await';
-- the calling Haskell thread waits for the result (the other threads keep
-- running).  A rejected Promise raises a JSException.
module GHC.Wasm.Prim(
  JSVal,
  JSString(..),
  toJSString, fromJSString,
  textToJSString, textFromJSString,
  JSException(..),
  freeJSVal,
  jsNull, jsUndefined, isNull, isUndefined,
  -- GHCJS style callbacks
  syncCallback, syncCallback1, syncCallback2, syncCallback3,
  syncCallback', syncCallback1', syncCallback2', syncCallback3',
  asyncCallback, asyncCallback1, asyncCallback2, asyncCallback3,
  ) where
import Prelude
import Control.Exception.Internal(JSException(..))
import qualified Data.ByteString as BS
import Data.Text(Text, pack, unpack)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Ptr(Ptr)
import Primitives(JSVal)
import System.IO.Unsafe(unsafePerformIO)

-- | A JavaScript string.
newtype JSString = JSString JSVal


-- JSException is defined in Control.Exception.Internal, since the runtime raises it.

-- | Drop the table entry of a JSVal, so the JavaScript value can be garbage collected.
-- The JSVal must not be used after this.
-- If the JSVal is a callback, the Haskell function is released as well.
foreign import ccall "js_free_jsval" freeJSVal :: JSVal -> IO ()

foreign import javascript "null" jsNull :: JSVal
foreign import javascript "undefined" jsUndefined :: JSVal
foreign import javascript "$1 === null" isNull :: JSVal -> Bool
foreign import javascript "$1 === undefined" isUndefined :: JSVal -> Bool

----------------------------------------
-- Strings, converted via UTF-8

-- The lengths are explicit, so a NUL character in the text survives the conversion
-- (TextDecoder decodes exactly the n bytes; UTF8ToString stops at a NUL in some
-- emscripten versions).  The decoder is created once, with ignoreBOM: a default
-- TextDecoder drops a leading U+FEFF (a byte order mark) from the decoded text.
foreign import javascript "(mhsjs.utf8Decoder || (mhsjs.utf8Decoder = new TextDecoder('utf-8', { ignoreBOM: true }))).decode(Module.HEAPU8.subarray($1, $1 + $2))" js_fromUTF8 :: Ptr a -> Int -> IO JSString
foreign import javascript "Module.lengthBytesUTF8($1)" js_utf8Length :: JSString -> IO Int
foreign import javascript "Module.stringToUTF8($1, $2, $3)" js_toUTF8 :: JSString -> Ptr a -> Int -> IO ()

textToJSString :: Text -> JSString
textToJSString t = unsafePerformIO $ BS.useAsCStringLen (encodeUtf8 t) (uncurry js_fromUTF8)

textFromJSString :: JSString -> Text
textFromJSString s = unsafePerformIO $ do
  n <- js_utf8Length s
  allocaBytes (n + 1) $ \ p -> do  -- stringToUTF8 also writes a terminating NUL
    js_toUTF8 s p (n + 1)
    decodeUtf8 <$> BS.packCStringLen (p, n)

toJSString :: String -> JSString
toJSString = textToJSString . pack

fromJSString :: JSString -> String
fromJSString = unpack . textFromJSString

----------------------------------------
-- Callbacks

-- | Make a JavaScript function that runs the Haskell action when called.
foreign import javascript "wrapper sync" syncCallback  :: IO () -> IO JSVal
foreign import javascript "wrapper sync" syncCallback1 :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper sync" syncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper sync" syncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal

-- | Make a JavaScript function that runs the Haskell action when called, and returns its result.
foreign import javascript "wrapper sync" syncCallback'  :: IO JSVal -> IO JSVal
foreign import javascript "wrapper sync" syncCallback1' :: (JSVal -> IO JSVal) -> IO JSVal
foreign import javascript "wrapper sync" syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
foreign import javascript "wrapper sync" syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal

-- | Make a JavaScript function that runs the Haskell action later (from the JavaScript event loop).
-- The JavaScript function returns a Promise.
foreign import javascript "wrapper" asyncCallback  :: IO () -> IO JSVal
foreign import javascript "wrapper" asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper" asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper" asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
