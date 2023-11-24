-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO.MD5(md5file) where
import Data.Word
import System.IO

md5file :: FilePath -> IO (Maybe [Word])
md5file _ = return (Just [])          -- dummy MD5
