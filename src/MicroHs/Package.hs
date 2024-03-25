module MicroHs.Package(
  PackageName, PackageVersion,
  Package(..),
  ) where
import MicroHs.Desugar(LDef)
import MicroHs.TypeCheck(TModule)

--
-- Packages are organized as follows.
-- The environment variable $PKGDIR determines the location,
-- with the default $HOME/.mcabal/packages
-- The file $PKGDIR/toc.txt the the table of contents.
-- Each line is the name of a package followed by the exported modules.
-- For each package 'foo' there is a serialized package
-- in $PKGDIR/foo.pkg
-- On startup the table of contents is read.
-- From this we get a map from module names to package file names.
-- On first use of a package module, we load the corresponding package file.
-- There is also a map from package names to loaded packages.
-- 


type PackageName = String
type PackageVersion = String

data Package = Package {
  pkgName      :: PackageName,                     -- package name
  pkgVersion   :: PackageVersion,                  -- package version
  pkgExported  :: [TModule [LDef]]                 -- exported modules
  pkgOther     :: [TModule [LDef]]                 -- non-exported modules
  pkgDepends   :: [(PackageName, PackageVersion)]  -- used packages
