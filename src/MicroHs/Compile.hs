-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# LANGUAGE QualifiedDo #-}
module MicroHs.Compile(module MicroHs.Compile) where
import Prelude --Xhiding (Monad(..), mapM, showString)
import qualified System.IO as IO
--Ximport Compat
--Ximport qualified CompatIO as IO

import qualified MicroHs.StringMap as M
import MicroHs.StateIO as S
import MicroHs.Desugar
import MicroHs.Parse
import MicroHs.TypeCheck

data Flags = Flags Int Bool [String] String
  --Xderiving (Show)

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
  ch <- execStateIO (compileModuleCached flags nm) (Cache [] M.empty)
  let
    defs m =
      case m of
        TModule _ _ _ ds -> ds
  IO.return $ concatMap defs $ M.elems $ cache ch

compileModuleCached :: Flags -> IdentModule -> StateIO Cache CModule
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
      cm <- compileModule flags nm
      S.when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing done " ++ showIdent nm
      c <- get
      put $ Cache (tail (working c)) (M.insert nm cm (cache c))
      S.return cm
    Just cm -> S.do
      S.when (verbose flags > 0) $
        liftIO $ putStrLn $ "importing cached " ++ showIdent nm
      S.return cm

compileModule :: Flags -> IdentModule -> StateIO Cache CModule
compileModule flags nm = S.do
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
  impMdls <- S.mapM (compileModuleCached flags) [ m | ImportSpec _ m _ <- specs ]
  let tmdl = typeCheck (zip specs impMdls) mdl
  liftIO $ putStr $ drop 1000000 $ showTModule showEDefs tmdl
  S.when (verbose flags > 2) $
    liftIO $ putStrLn $ "type checked:\n" ++ showTModule showEDefs tmdl ++ "-----\n"
  let dmdl = desugar tmdl
  S.when (verbose flags > 2) $
    liftIO $ putStrLn $ "desugared:\n" ++ showTModule showLDefs dmdl
  S.return dmdl

------------------

readFilePath :: [String] -> String -> IO String
readFilePath path name = IO.do
  h <- openFilePath path name
  IO.hGetContents h

openFilePath :: [String] -> String -> IO IO.Handle
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
