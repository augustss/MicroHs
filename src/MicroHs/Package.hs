module MicroHs.Package(
  IdentPackage,
  Package(..),
  forcePackage,
  ) where
import Prelude(); import MHSPrelude
import Data.Version
import MicroHs.Desugar(LDef)
import MicroHs.Ident(Ident)
import MicroHs.MRnf
import MicroHs.TypeCheck(TModule, GlobTables)

--
-- Packages are organized as follows:
-- There is a package search path (default is ~/.mcabal/mhs-VERSION/)
-- In this directory there is a subdirectory, packages, that contains a
-- serialized Package for each installed package.
-- There is also a file for each exported module that contains just
-- the package name.
-- So if we have a package foo.pkg, exporting modules Foo.Bar and Foo.Baz
-- we would have the following directory structure
--   packages/foo.pkg
--   Foo/Bar.txt
--   Foo/Baz.txt
-- The files Foo/Bar.txt and Foo/Baz.txt will contain simply "foo.pkg".
-- 

type IdentPackage = Ident

data Package = Package {
  pkgName      :: IdentPackage,                    -- package name
  pkgVersion   :: Version,                         -- package version
  pkgCompiler  :: String,                          -- compiler version that created the package
  pkgExported  :: [TModule [LDef]],                -- exported modules
  pkgOther     :: [TModule [LDef]],                -- non-exported modules
  pkgTables    :: GlobTables,                      -- global tables
  pkgDepends   :: [(IdentPackage, Version)]        -- used packages
  }
  -- deriving (Show)

instance MRnf Package where
  mrnf (Package a b c d e f g) = mrnf a `seq` mrnf b `seq` mrnf c `seq` mrnf d `seq` mrnf e `seq` mrnf f `seq` mrnf g

-- Fully evaluate a package
forcePackage :: Package -> Package
forcePackage p = mrnf p `seq` p

