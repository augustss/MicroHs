module Install where
import Control.Monad
import Data.Maybe
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Directory
import System.FilePath
import System.Process
import MicroHs.Config

type CConf = [(Key, Value)]

confFile :: FilePath
confFile = "mhs.conf"

theConfig :: String
theConfig | _isWindows = "windows"
          | otherwise  = "unix"

rts :: String
rts = "src" </> "runtime"

doIt :: Bool
doIt = False

main :: IO ()
main = do
  home <- getHomeDirectory
  confText <- readFile confFile
  let conf = either (\ s -> error $ "cannot parse config " ++ s) id $
                    parseConfig confFile confText
      cconf = fromMaybe (error $ "Cannot locate section " ++ theConfig) $
              lookup theConfig conf
      mcabal = home </> ".mcabal"
      mcabalBin = mcabal </> "bin"
      exes = ["mhs", "cpphs", "mcabal"]
  --
  mkdir $ "bin"
  mapM_ (buildBin cconf) exes
  machdep $ rts </> "MachDeps.h"
  mkdir mcabalBin
  let cpbin pgm = let exe = pgm <.> exeSuffix in copy ("bin" </> exe) (mcabalBin </> exe)
  mapM_ cpbin exes
  version <- init <$> mhsOut ["--numeric-version"]
  let mCabalMhs = mcabal </> ("mhs-" ++ version)
      mData = mCabalMhs </> "packages" </> ("mhs-" ++ version) </> "data"
  mkdir mData
  copy "mhs.conf" (mData </> "mhs.conf")

{-
MCABALMHS=$(MCABAL)/mhs-$(VERSION)
MDATA=$(MCABALMHS)/packages/mhs-$(VERSION)/data
	cp -r $(RTS)/* $(MRUNTIME)
	@mkdir -p $(MCABALMHS)
	bin/mhs -Q generated/base.pkg $(MCABALMHS)
	@echo $$PATH | tr ':' '\012' | grep -q $(MCABALBIN) || echo '***' Add $(MCABALBIN) to the PATH
-}

buildBin :: CConf -> String -> IO ()
buildBin cconf pgm = do
  let src = "generated" </> pgm <.> ".c"
      dst = "bin" </> pgm <.> exeSuffix
  cc cconf [get cconf "ccflags", "-I" ++ rts, "-I" ++ (rts </> get cconf "conf"),
            rts </> "main.c", rts </> "eval.c",
            src, get cconf "cclibs", getD cconf "-o" "cout" ++ dst]

mhsOut :: [String] -> IO String
mhsOut args =
  readProcess ("bin" </> "mhs" <.> exeSuffix) args ""

-----

getD :: CConf -> Value -> Key -> Value
getD cconf def key = fromMaybe def $ lookup key cconf

get :: CConf -> Key -> Value
get cconf key = getD cconf (error $ "Cannot find " ++ key) key

-----

cc :: CConf -> [String] -> IO ()
cc cconf args = do
  let c = get cconf "cc"
  msg $ unwords (c : args)
  when doIt $
    callProcess c args

exeSuffix :: String
exeSuffix | _isWindows = "exe"
          | otherwise  =  ""

msg :: String -> IO ()
msg s = putStrLn s

-----

mkdir :: FilePath -> IO ()
mkdir dir = do
  msg $ "mkdir " ++ dir
  when doIt $
    createDirectoryIfMissing True dir

machdep :: FilePath -> IO ()
machdep name = do
  msg $ "create " ++ name
  big <- isBigEndian
  when doIt $
    writeFile name $ unlines
      [ "#define WORD_SIZE_IN_BITS " ++ show _wordSize
      , (if big then "#define" else "#undef") ++ " WORDS_BIGENDIAN"
      ]

isBigEndian :: IO Bool
isBigEndian = do
  let w :: Word
      w = if _wordSize == 32 then 0x01000002 else 0x0100000000000002
  p <- new w
  b <- peek (castPtr p :: Ptr Word8)
  return (b == 1)

copy :: FilePath -> FilePath -> IO ()
copy src dst = do
  msg $ unwords ["cp", src, dst]
  when doIt $ do
    copyFile src dst
    copyPermissions src dst
