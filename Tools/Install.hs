module Install where
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import Data.Maybe
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.TimeMilli
import Text.Printf
import MicroHs.Config

type CConf = [(Key, Value)]


main :: IO ()
main = do
  start <- getTimeMilli
  args <- getArgs
  let flags = decodeArgs defaultFlags args
  confText <- macroExpand flags <$> readFile (confFile flags)
  home <- getHomeDirectory
  let conf = either (\ s -> error $ "cannot parse config " ++ s) id $
                    parseConfig (confFile flags) confText
      cc = fromMaybe (error $ "Cannot locate section " ++ target flags) $
              lookup (target flags) conf
      exe | target flags == "windows" = "exe"
          | otherwise                 =  ""
      inst = fromMaybe (home </> ".mcabal") (instDirM flags)
      flags' = flags { exeSuffix = exe, cconf = cc, instDir = inst, conf = confText }

  install flags'

  end <- getTimeMilli
  let delta = (end - start) `quot` 100
      (sec, tenths) = delta `quotRem` 10
  putStrLn $ "Install time: " ++ show sec ++ "." ++ show tenths ++ "s"

install :: Flags -> IO ()
install flags = do
  time "instBin" $ instBin flags
  vers <- init <$> mhsOut flags ["--numeric-version"]
  let flags' = flags { version = vers }
  time "mkMachdep" $ mkMachdep flags'
  time "mkConf" $ mkConf flags'
  time "copyRTS" $ copyRTS flags'
  time "copyBase" $ copyBase flags'
  time "checkPath" $ checkPath flags'

instBin :: Flags -> IO ()
instBin flags = do
  let mCabal = instDir flags
      mcabalBin = mCabal </> "bin"
      exes = ["mhs", "cpphs", "mcabal"]
  mkdir flags "bin"
  mapM_ (buildBin flags) exes
  mkdir flags mcabalBin
  let cpbin pgm = do
        let exe = pgm <.> exeSuffix flags
        copy flags ("bin" </> exe) (mcabalBin </> exe)
  mapM_ cpbin exes

mkMachdep :: Flags -> IO ()
mkMachdep flags =
  machdep flags $ "src" </> "runtime" </> "MachDeps.h"

mkConf :: Flags -> IO ()
mkConf flags = do
  msg flags "create mhs.conf"
  unless (dryRun flags) $
    writeFile "mhs.conf" (conf flags)

copyRTS :: Flags -> IO ()
copyRTS flags = do
  let mCabalMhs = instDir flags </> ("mhs-" ++ version flags)
  let mData = mCabalMhs </> "packages" </> ("mhs-" ++ version flags) </> "data"
      rts = "src" </> "runtime"
  time "mkdir eData" $ mkdir flags mData
  time "copy mhs.conf" $ copy flags "mhs.conf" (mData </> "mhs.conf")
  time "copyDir" $ copyDir flags rts (mData </> rts)

copyBase :: Flags -> IO ()
copyBase flags = do
  let mCabalMhs = instDir flags </> ("mhs-" ++ version flags)
  mkdir flags mCabalMhs
  out <- mhsOut flags ["-Q", "generated" </> "base.pkg", mCabalMhs]
  msg flags out

checkPath :: Flags -> IO ()
checkPath flags = do
  path <- fromMaybe "" <$> lookupEnv "PATH"
  let paths = splitOn pathSep path
      pathSep | target flags == "windows" = ";"
              | otherwise = ":"
      bin = instDir flags </> "bin"
  unless (bin `elem` paths) $
    putStrLn $ "Please add " ++ bin ++ " to your PATH"

buildBin :: Flags -> String -> IO ()
buildBin flags pgm = do
  let src = "generated" </> pgm <.> ".c"
      dst = "bin" </> pgm <.> exeSuffix flags
      ccf = cconf flags
      rts = "src" </> "runtime"
  time ("buildBin " ++ pgm) $
   cc flags [get ccf "ccflags", "-I" ++ rts, "-I" ++ (rts </> get ccf "conf"),
            rts </> "main.c", rts </> "eval.c",
            src, get ccf "cclibs", getD ccf "-o" "cout" ++ dst]

mhsOut :: Flags -> [String] -> IO String
mhsOut flags args = do
  let exe = "bin" </> "mhs" <.> exeSuffix flags
  msg flags $ unwords (exe:args)
  if dryRun flags then
    return "MHSOUT\n"
   else
    readProcess exe args ""

-----

getD :: CConf -> Value -> Key -> Value
getD cconf def key = fromMaybe def $ lookup key cconf

get :: CConf -> Key -> Value
get cconf key = getD cconf (error $ "Cannot find " ++ key) key

-----

cc :: Flags -> [String] -> IO ()
cc flags args = do
  let c = get (cconf flags) "cc"
  msg flags $ unwords (c : args)
  unless (dryRun flags) $
    callProcess c args

msg :: Flags -> String -> IO ()
msg flags s | not (quiet flags) = putStrLn s
            | otherwise         = return ()

-----

mkdir :: Flags -> FilePath -> IO ()
mkdir flags dir = do
  msg flags $ "mkdir " ++ dir
  unless (dryRun flags) $ do
    _ <- system $ "mkdir -p " ++ dir
    return ()
--    createDirectoryIfMissing True dir

machdep :: Flags -> FilePath -> IO ()
machdep flags name = do
  msg flags $ "create " ++ name
  big <- isBigEndian
  unless (dryRun flags) $
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

copy :: Flags -> FilePath -> FilePath -> IO ()
copy flags src dst = do
  msg flags $ unwords ["cp", src, dst]
  unless (dryRun flags) $ do
    copyFileBS src dst
    copyPermissions src dst

copyDir :: Flags -> FilePath -> FilePath -> IO ()
copyDir flags src dst = do
  msg flags $ unwords ["cp -r", src, dst]
  let flags' = flags{ quiet = not (verbose flags) }
  mkdir flags dst
  let one file = do
        d <- doesDirectoryExist (src </> file)
        (if d then copyDir else copy) flags' (src </> file) (dst </> file)
  mapM_ one =<< listDirectory src

splitOn :: String -> String -> [String]
splitOn d = loop []
  where loop [] [] = []
        loop r [] = [reverse r]
        loop r s@(c:cs) | Just s' <- stripPrefix d s = reverse r : loop [] s'
                        | otherwise = loop (c:r) cs

macroExpand :: Flags -> String -> String
macroExpand flags = loop
  where loop [] = []
        loop ('%':cs) =
          let (mn, rest) = span nameChar cs
              nameChar c = isAlphaNum c || c == '_'
          in  fromMaybe "" (lookup mn (macros flags)) ++ loop rest
        loop (c:cs) = c : loop cs

-----

data Flags = Flags
  { target    :: String
  , dryRun    :: Bool
  , verbose   :: Bool
  , quiet     :: Bool
  , macros    :: [(String, String)]
  , goals     :: [String]
  , confFile  :: FilePath
  , instDirM  :: Maybe FilePath
  -- The rest are for internal use
  , exeSuffix :: String
  , instDir   :: FilePath
  , cconf     :: CConf
  , version   :: String
  , conf      :: String
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags =
  Flags
    { target    = if _isWindows then "windows" else "unix"
    , dryRun    = False
    , verbose   = False
    , quiet     = False
    , macros    = []
    , goals     = []
    , confFile  = "mhs.conf.in"
    , instDirM  = Nothing
    --
    , exeSuffix = undefined
    , instDir   = undefined
    , cconf     = undefined
    , version   = undefined
    , conf      = undefined
    }

decodeArgs :: Flags -> [String] -> Flags
decodeArgs f [] = f
decodeArgs f (arg:args) =
  case arg of
    "--help"           -> error usage
    "-v"               -> decodeArgs f{verbose = True} args
    "-q"               -> decodeArgs f{quiet = True} args
    "--dryrun"         -> decodeArgs f{dryRun = True} args
    '-':'t':s          -> decodeArgs f{target = s} args
    '-':'i':s          -> decodeArgs f{instDirM = Just s} args
    '-':_              -> error $ "Unknown flag: " ++ arg ++ "\n" ++ usage
    _ | (m, '=':e) <- span (/= '=') arg
                       -> decodeArgs f{macros = macros f ++ [(m, e)]} args
      | otherwise      -> decodeArgs f{goals = goals f ++ [arg]} args

usage :: String
usage = "\ninstall [--help] [-v] [--dryrun] [-tTARGET] [-iDIR] [NAME=MACRO] [GOAL]\n"

copyFileBS :: FilePath -> FilePath -> IO ()
copyFileBS src dst = BS.readFile src >>= BS.writeFile dst

time :: String -> IO a -> IO a
time _ ioa | 0==0 = ioa
-- Time individual operations
time s ioa = do
  t1 <- getTimeMilli
  a <- ioa
  t2 <- getTimeMilli
  printf "***** %s %fs\n" s (fromIntegral (t2 - t1) / 1000)
  return a
