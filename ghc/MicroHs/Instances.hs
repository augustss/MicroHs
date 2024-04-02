{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module MicroHs.Instances(getMhsDir, compiledWithGHC) where
import Control.DeepSeq
import System.Environment
import MicroHs.CompileCache
import MicroHs.Ident
import MicroHs.Exp
import MicroHs.Expr
#if !defined(NOTCABAL)
import Paths_MicroHs

getMhsDir :: IO (Maybe FilePath)
getMhsDir = do
  md <- lookupEnv "MHSDIR"
  case md of
    Just _ -> return md
    Nothing -> Just <$> getDataDir
#else

getMhsDir :: IO (Maybe FilePath)
getMhsDir = lookupEnv "MHSDIR"

#endif

instance NFData Cache where rnf _ = ()

instance NFData Exp where rnf (Var i) = rnf i; rnf (App f a) = rnf f `seq` rnf a; rnf (Lam i e) = rnf i `seq` rnf e; rnf (Lit l) = rnf l

instance NFData Lit where rnf (LInt i) = rnf i; rnf (LInteger i) = rnf i; rnf (LDouble d) = rnf d; rnf (LRat r) = rnf r; rnf (LChar c) = rnf c; rnf (LStr s) = rnf s; rnf (LPrim s) = rnf s; rnf (LExn s) = rnf s; rnf (LForImp s _) = rnf s; rnf (LTick s) = rnf s; rnf (LUStr s) = rnf s

instance NFData Ident where rnf (Ident _ s) = rnf s

compiledWithGHC :: Bool
compiledWithGHC = True
