-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Compile(module MicroHs.Compile) where
import Prelude --Xhiding (Monad(..), mapM, showString)
import qualified System.IO as IO
--Ximport Compat
--Ximport qualified CompatIO as IO
--Ximport System.IO(Handle)

import qualified MicroHs.StringMap as M
import MicroHs.StateIO as S
import MicroHs.Desugar
import MicroHs.Parse
import MicroHs.TypeCheck

data Flags = Flags Int Bool [String] String
  --Xderiving (Show)

type Time = Int

verbose :: Flags -> Int
verbose f =
  case f of
    Flags x _ _ _ -> x

runIt :: Flags -> Bool
runIt f =
  case f of
    Flags _ x _ _ -> x

paths :: Flags -> [String]
paths f =
  case f of
    Flags _ _ x _ -> x

output :: Flags -> String
output f =
  case f of
    Flags _ _ _ x -> x

-----------------

type CModule = TModule [LDef]
data Cache = Cache [IdentModule] (M.Map CModule)
  --Xderiving (Show)

working :: Cache -> [IdentModule]
working c =
  case c of
    Cache x _ -> x

updWorking :: [IdentModule] -> Cache -> Cache
updWorking w c =
  case c of
    Cache _ m -> Cache w m

cache :: Cache -> M.Map CModule
cache c =
  case c of
    Cache _ x -> x

{-
updCache :: M.Map Module -> Cache -> Cache
updCache x c =
  case c of
    Cache w _ -> Cache w x
-}

-----------------

compile :: Flags -> IdentModule -> IO [LDef]
compile flags nm = IO.do
  ((_, t), ch) <- runStateIO (compileModuleCached flags nm) (Cache [] M.empty)
  let
    defs m =
      case m of
        TModule _ _ _ _ ds -> ds
  IO.when (verbose flags > 0) $
    putStrLn $ "total import time     " ++ padLeft 6 (showInt t) ++ "ms"
  IO.return $ concatMap defs $ M.elems $ cache ch

-- Compile a module with the given name.
-- If the module has already been compiled, return the caches result.
compileModuleCached :: Flags -> IdentModule -> StateIO Cache (CModule, Time)
compileModuleCached flags nm = S.do
  ch <- gets cache
  case M.lookup nm ch of
    Nothing -> S.do
      ws <- gets working
      S.when (elemBy eqIdent nm ws) $
        error $ "recursive module: " ++ showIdent nm
      modify $ \ c -> updWorking (nm : working c) c
      S.when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing " ++ showIdent nm
      (cm, tp, tt, ts) <- compileModule flags nm
      S.when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing done " ++ showIdent nm ++ ", " ++ showInt (tp + tt) ++ "ms (" ++ showInt tp ++ " + " ++ showInt tt ++ ")"
      c <- get
      put $ Cache (tail (working c)) (M.insert nm cm (cache c))
      S.return (cm, tp + tt + ts)
    Just cm -> S.do
      S.when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing cached " ++ showIdent nm
      S.return (cm, 0)

-- Find and compile a module with the given name.
-- The times are (parsing, typecheck+desugar, imported modules)
compileModule :: Flags -> IdentModule -> StateIO Cache (CModule, Time, Time, Time)
compileModule flags nm = S.do
  t1 <- liftIO getTimeMilli
  let
    fn = map (\ c -> if eqChar c '.' then '/' else c) nm ++ ".hs"
  mdl <- S.fmap (parseDie pTop fn) (liftIO (readFilePath (paths flags) fn))
  --liftIO $ putStrLn $ showEModule mdl
  let
    EModule nmn _ defs = mdl
  S.when (not (eqIdent nm nmn)) $
    error $ "module name does not agree with file name: " ++ showIdent nm
  let
    specs = [ s | Import s <- defs ]
  t2 <- liftIO getTimeMilli
  (impMdls, ts) <- S.fmap unzip $ S.mapM (compileModuleCached flags) [ m | ImportSpec _ m _ <- specs ]
  t3 <- liftIO getTimeMilli
  let
    tmdl = typeCheck (zip specs impMdls) mdl
  S.when (verbose flags > 2) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let
    dmdl = desugar tmdl
  liftIO $ putStr $ drop 1000000 $ showTModule showLDefs dmdl
  t4 <- liftIO getTimeMilli
  S.when (verbose flags > 2) $
    (liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl)
  S.return (dmdl, t2-t1, t4-t3, sum ts)

------------------

readFilePath :: [String] -> String -> IO String
readFilePath path name = IO.do
  h <- openFilePath path name
  IO.hGetContents h

openFilePath :: [String] -> String -> IO Handle
openFilePath adirs fileName =
  case adirs of
    [] -> error $ "File not found: " ++ showString fileName
    dir:dirs -> IO.do
      let
        path = dir ++ "/" ++ fileName
      mh <- openFileM path IO.ReadMode
      case mh of
        Nothing -> openFilePath dirs fileName -- If opening failed, try the next directory
        Just hdl -> IO.return hdl
