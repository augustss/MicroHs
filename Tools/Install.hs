module Install where
import Data.Word
import Foreign.Storable

mhs :: [String] -> IO String
mhs args = readProcess ("bin" </> "mhs" <.> exe) args ""

exe :: String
exe | _isWindows = "exe"
    | otherwise  = ""

dirDelim :: String
dirDelim | _isWindows = "\\"
         | otherwise  = "/"

(<.>) :: FilePath -> String -> FilePath
p <.> "" = p
p <.> s  = p ++ "." ++ s

(</>) :: FilePath -> FilePath -> FilePath
"" </> s = s
p  </> s = p ++ (dirDelim : s)

copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive srcDir dstDir = undefined

copyFileWithPermissionToDir :: FilePath -> FilePath -> FilePath -> IO ()
copyFileWithPermissionToDir srcDir dstDir file = undefined

main :: IO ()
main = do
  mhsVersion <- ("mhs" ++) <$>mhs ["--numeric-version"]
  args <- getArgs
  let mCabal =
        case args of
          [s] -> s
          _ -> error "Usage: install TARGETDIR"
      mCabalBin = mCabal </> "bin"
      mCabalMhs = mCabal </> mhsVersion
      mData     = mCabalMhs </> "packages" </> mhsVersion </> "data"
      mRuntime  = mData </> "src" </> "runtime"
  createDirectoryIfMissing True $ mCabalBin
  createDirectoryIfMissing True $ mCabalMhs
  createDirectoryIfMissing True $ mRuntime
  copyFileWithPermissionToDir "bin" mCabalBin ("mhs"    <.> exe)
  copyFileWithPermissionToDir "bin" mCabalBin ("mcabal" <.> exe)
  copyFileWithPermissionToDir "bin" mCabalBin ("cpphs"  <.> exe)
  copyFileWithPermissionToDir ""    mData     "mhs.conf"
  copyRecursive               ("src" </> "runtime") mRuntime
  genMachdep (mRuntime </> "MachDeps.h")
  _ <- mhs ["-Q", "generated" </> "base.pkg", mCabalMhs]
  return ()

genMachdep :: FilePath -> IO ()
genMachdep mdep = do
  let
    w :: Word
    w = if _wordSize == 32 then 0x01000002 else 0x0100000000000002
  p <- new w
  b <- peek (castPtr p :: Ptr Word8)
  let bigEndian = b == 1
  writeFile mdep $ unlines
    [ "#define WORD_SIZE_IN_BITS " ++ show _wordSize,
      if bigEndian then "#define WORDS_BIGENDIAN 1" else "#undef WORDS_BIGENDIAN"
    ]
