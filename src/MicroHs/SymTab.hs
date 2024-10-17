module MicroHs.SymTab(
  Entry(..), entryType,
  SymTab,
  stEmpty,
  stLookup,
  stFromList,
  stInsertMany,
  stInsertGlbU,
  stInsertGlbQ,
  stElemsLcl,
  stKeysLcl,
  stKeysGlbU,
  stInsertLcl,
  mapMSymTab,
  ) where
import Prelude(); import MHSPrelude
import Control.Applicative
import Data.List
import MicroHs.Builtin(builtinMdl)
import MicroHs.Expr(Expr(..), EType, conIdent)
import MicroHs.Ident(Ident, showIdent, unIdent, mkIdentSLoc, slocIdent)
import MicroHs.List
import qualified MicroHs.IdentMap as M

-- Symbol table
-- 

-- Symbol table entry for symbol i.
data Entry = Entry
  Expr             -- convert (EVar i) to this expression; sometimes just (EVar i)
  EType            -- type/kind of identifier
--  deriving(Show)

instance Show Entry where
  showsPrec _ (Entry e t) = showsPrec 0 e . showString " :: " . showsPrec 0 t

instance Eq Entry where
  Entry x _ == Entry y _  =  getIdent x == getIdent y

getIdent :: Expr -> Ident
getIdent ae =
  case ae of
    EVar i -> i
    ECon c -> conIdent c
    EApp f _ -> getIdent f
    _ -> error "getIdent"

entryType :: Entry -> EType
entryType (Entry _ t) = t

-----------------------------------------------

-- The symbol table is split into 3 parts
--  * locals        there is usually only a few of these,
--                  so linear search is faster than a tree map
--  * unqualified   most identifiers are used unqualified, so
--        globals   it's better not to keep a globals in one table
--  * qualified     as a last resort, look among the qualified globals
--      globals
data SymTab = SymTab {
  _lcl  :: [(Ident, Entry)],     -- locals
  _uglb :: M.Map [Entry],        -- unqualified globals
  _qglb :: M.Map [Entry]         -- qualified globals
  }
--  deriving(Show)

instance Show SymTab where
  show (SymTab l ug qg) = unlines $
    ("Locals:"  : map (("  " ++) . show) l) ++
    ("UGlobals:" : map (("  " ++) . show) (M.toList ug)) ++
    ("QGlobals:" : map (("  " ++) . show) (M.toList qg))
  
mapMSymTab :: forall m . Monad m => (Entry -> m Entry) -> SymTab -> m SymTab
mapMSymTab f (SymTab l ug qg) = do
  l' <- mapM (\ (i, a) -> (,) i <$> f a) l
  ug' <- M.mapM (mapM f) ug
  qg' <- M.mapM (mapM f) qg
  return $ SymTab l' ug' qg'

stEmpty :: SymTab
stEmpty = SymTab [] M.empty M.empty

stLookup :: String -> Ident -> SymTab -> Either String Entry
stLookup msg i (SymTab l ug qg) =
  case lookup i l of
    Just e -> Right e
    Nothing ->
      case M.lookup i ug <|> M.lookup i qg <|> M.lookup (hackBuiltin i) ug of
        Just [e] -> Right e
        Just es  -> Left $ "ambiguous " ++ msg ++ ": " ++ showIdent i ++ " " ++
                           showListS showIdent [ getIdent e | Entry e _ <- es ]
        Nothing  -> Left $ "undefined " ++ msg ++ ": " ++ showIdent i
                           -- ++ "\n" ++ show lenv ++ "\n" ++ show genv

-- When a module uses 'import Prelude()' the Mhs.Builtin (aka B@) will
-- also not be imported.  So as a last recourse, look for the identifier
-- unqualified.
hackBuiltin :: Ident -> Ident
hackBuiltin i | Just ('.':s) <- stripPrefix builtinMdl (unIdent i) = mkIdentSLoc (slocIdent i) s
hackBuiltin i = i

stFromList :: [(Ident, [Entry])] -> [(Ident, [Entry])] -> SymTab
stFromList us qs = SymTab [] (M.fromListWith union us) (M.fromListWith union qs)

stInsertMany :: [(Ident, [Entry])] -> [(Ident, [Entry])] -> SymTab -> SymTab
stInsertMany us qs (SymTab lcl sus sqs) = SymTab lcl (M.insertManyWith union us sus) (M.insertManyWith union qs sqs)

stElemsLcl :: SymTab -> [Entry]
stElemsLcl (SymTab l _ _) = map snd l

stKeysLcl :: SymTab -> [Ident]
stKeysLcl (SymTab l _ _) = map fst l

stKeysGlbU :: SymTab -> [Ident]
stKeysGlbU (SymTab _ m _) = M.keys m

stInsertLcl :: Ident -> Entry -> SymTab -> SymTab
stInsertLcl i a (SymTab l ug qg) = SymTab ((i, a) : l) ug qg

stInsertGlbU :: Ident -> [Entry] -> SymTab -> SymTab
stInsertGlbU i as (SymTab l ug qg) = SymTab l (M.insertWith union i as ug) qg

stInsertGlbQ :: Ident -> [Entry] -> SymTab -> SymTab
stInsertGlbQ i as (SymTab l ug qg) = SymTab l ug (M.insertWith union i as qg)
