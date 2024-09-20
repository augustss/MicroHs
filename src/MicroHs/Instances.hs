-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Instances(compiledWithGHC) where
-- For GHC compatibility
import Prelude(); import MHSPrelude
import MicroHs.CompileCache
import MicroHs.Ident
import MicroHs.Exp
import MicroHs.Expr
import Compat

compiledWithGHC :: Bool
compiledWithGHC = False

instance NFData Cache
instance NFData Exp
instance NFData Lit
instance NFData Ident
