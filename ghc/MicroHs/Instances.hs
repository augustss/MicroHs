{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module MicroHs.Instances(compiledWithGHC) where
import Control.DeepSeq
import MicroHs.CompileCache
import MicroHs.Ident
import MicroHs.Exp
import MicroHs.Expr

instance NFData Cache where rnf _ = ()

instance NFData Exp where rnf (Var i) = rnf i; rnf (App f a) = rnf f `seq` rnf a; rnf (Lam i e) = rnf i `seq` rnf e; rnf (Lit l) = rnf l

instance NFData Lit where rnf (LInt i) = rnf i; rnf (LInteger i) = rnf i; rnf (LDouble d) = rnf d; rnf (LRat r) = rnf r; rnf (LChar c) = rnf c; rnf (LStr s) = rnf s; rnf (LPrim s) = rnf s; rnf (LExn s) = rnf s; rnf (LForImp s _) = rnf s; rnf (LTick s) = rnf s; rnf (LUStr s) = rnf s

instance NFData SLoc where rnf (SLoc f l c) = rnf f `seq` rnf l `seq` rnf c

instance NFData Ident where rnf i = rnf (slocIdent i)

compiledWithGHC :: Bool
compiledWithGHC = True
