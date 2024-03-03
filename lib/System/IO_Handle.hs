module System.IO_Handle(BFILE, Handle(..)) where
import Prelude()
import Primitives

data BFILE
newtype Handle = Handle (Ptr BFILE)
