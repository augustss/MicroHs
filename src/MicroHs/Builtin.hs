module MicroHs.Builtin(
  builtinMdl,
  mkBuiltin,
  mkBuiltinQ,
  ) where
import Prelude(); import MHSPrelude
import MicroHs.Ident

-- The compiler needs a number of identifiers from libraries.
-- These are make available by (programatically) adding
--  'import Mhs.Builtin qualified as B@"
-- The name 'B@' is not a valid identifier, so these name
-- cannot be used accidentally in user code.
builtinMdl :: String
builtinMdl = "B@"
builtinMdlQ :: String
builtinMdlQ = "Mhs.Builtin"

-- Identifier for a builtin that will be renamed.
mkBuiltin :: SLoc -> String -> Ident
mkBuiltin loc name = mkIdentSLoc loc ((builtinMdl ++ ".") ++ name)

-- Identifier for a builtin that is alread renamed.
mkBuiltinQ :: SLoc -> String -> Ident
mkBuiltinQ loc name = mkIdentSLoc loc ((builtinMdlQ ++ ".") ++ name)