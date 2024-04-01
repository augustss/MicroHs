module MicroHs.SymTab(
  Entry(..), entryType,
  SymTab,
  stLookup,
  stFromList,
  stInsertGlbU,
  stInsertGlbQ,
  stElemsLcl,
  stKeysLcl,
  stInsertLcl,
  mapMSymTab,
  ) where
import Control.Applicative
import Data.List
import MicroHs.Expr(Expr(..), EType, conIdent)
import MicroHs.Ident(Ident, showIdent)
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

stLookup :: String -> Ident -> SymTab -> Either String Entry
stLookup msg i (SymTab l ug qg) =
  case lookup i l of
    Just e -> Right e
    Nothing ->
      case M.lookup i ug <|> M.lookup i qg of
        Just [e] -> Right e
        Just es  -> Left $ "ambiguous " ++ msg ++ ": " ++ showIdent i ++ " " ++
                           showListS showIdent [ getIdent e | Entry e _ <- es ]
        Nothing  -> Left $ "undefined " ++ msg ++ ": " ++ showIdent i
                           -- ++ "\n" ++ show lenv ++ "\n" ++ show genv

stFromList :: [(Ident, [Entry])] -> [(Ident, [Entry])] -> SymTab
stFromList us qs = SymTab [] (M.fromListWith union us) (M.fromListWith union qs)

stElemsLcl :: SymTab -> [Entry]
stElemsLcl (SymTab l _ _) = map snd l

stKeysLcl :: SymTab -> [Ident]
stKeysLcl (SymTab l _ _) = map fst l

stInsertLcl :: Ident -> Entry -> SymTab -> SymTab
stInsertLcl i a (SymTab l ug qg) = SymTab ((i, a) : l) ug qg

stInsertGlbU :: Ident -> [Entry] -> SymTab -> SymTab
stInsertGlbU i as (SymTab l ug qg) = SymTab l (M.insertWith union i as ug) qg

stInsertGlbQ :: Ident -> [Entry] -> SymTab -> SymTab
stInsertGlbQ i as (SymTab l ug qg) = SymTab l ug (M.insertWith union i as qg)
