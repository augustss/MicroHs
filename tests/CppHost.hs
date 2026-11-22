{-# LANGUAGE CPP #-}
-- As in GHC, the preprocessor defines os_HOST_OS and arch_HOST_ARCH (and
-- the _BUILD_ versions), with the names from System.Info.
module CppHost(main) where
import System.Info (arch, os)

cppOS :: String
#if defined(darwin_HOST_OS) && defined(darwin_BUILD_OS)
cppOS = "darwin"
#elif defined(linux_HOST_OS) && defined(linux_BUILD_OS)
cppOS = "linux"
#elif defined(mingw32_HOST_OS) && defined(mingw32_BUILD_OS)
cppOS = "mingw32"
#elif defined(windows_HOST_OS) && defined(windows_BUILD_OS)
cppOS = "windows"
#elif defined(freebsd_HOST_OS) && defined(freebsd_BUILD_OS)
cppOS = "freebsd"
#else
cppOS = "unknown"
#endif

cppArch :: String
#if defined(aarch64_HOST_ARCH) && defined(aarch64_BUILD_ARCH)
cppArch = "aarch64"
#elif defined(x86_64_HOST_ARCH) && defined(x86_64_BUILD_ARCH)
cppArch = "x86_64"
#elif defined(i386_HOST_ARCH) && defined(i386_BUILD_ARCH)
cppArch = "i386"
#elif defined(arm_HOST_ARCH) && defined(arm_BUILD_ARCH)
cppArch = "arm"
#else
cppArch = "unknown"
#endif

main :: IO ()
main = do
  print (cppOS == os)
  print (cppArch == arch)
