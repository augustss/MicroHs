-- Copyright 2026 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Extracting the code of a function: the part of the graph of its value
-- that is code, i.e., the partial applications that make up the function.
-- The graph is read directly: nodeAddr gives the address of a node, and the
-- fields are read with peek.  See NODEPTR in eval.c for the encoding of the
-- nodes, and rts_info for the tables that describe them.
--
-- The graph is traversed depth first from the root, and the application
-- nodes that are code, i.e., partial applications, are copied.
-- The traversal stops at
--  - data: a value, or a constructor with its fields;
--  - a redex, i.e., an application with as many arguments as its head needs;
--    it can be shared, so the copy refers to it instead;
--  - a node that starts a cycle, other than the root.  It is a function of
--    its own, which can be extracted by itself.  That a node starts a cycle
--    is only known when the traversal comes back to it, so it is copied
--    first; when the traversal is done with it, its copy and everything
--    after it is dropped, and it becomes a reference;
--  - a node beyond the exploration limit: exploreFactor times the size limit
--    copies made, counting the ones that are dropped, so a graph with many
--    cycles cannot make it explore the same nodes over and over.
-- The table of the copies is also the table of visited nodes, so sharing is
-- kept.  A cycle back to the root is a reference to the root's copy, entry 0.
--
-- Finding the cycles needs a depth first traversal, but cutting the copy at
-- the size limit in depth first order would spend it on whatever is first,
-- e.g., the rotations of a balance function, and cut the tests near the root.
-- So the traversal explores more, and then select keeps the copied nodes that
-- are nearest the root, breadth first, up to the size limit.
--
-- The graph must not change while it is read: nothing may evaluate the
-- redexes in it.  The GC does not move nodes, but it removes indirections,
-- and then frees them.  So indirections are skipped at once, and a field is
-- read from its node just before the node it points to is visited.
module Mhs.Extract(
  Cell, nodeAddr,
  Extraction(..), EntryNo, Entry(..), Why(..), Opnd(..),
  defaultLimit, extract, extractionProg, extractionSummary,
  ) where
import Control.Monad
import Data.Bits
import Data.ByteString(packCStringLen)
import Data.Char(toLower)
import Data.Maybe(fromMaybe, isJust)
import Data.Word(Word8, Word16)
import Foreign.C.String(CString, peekCString)
import Foreign.C.Types(CInt)
import Foreign.Ptr
import Foreign.Storable
import Mhs.IntMap(IntMap)
import qualified Mhs.IntMap as M
import Mhs.Reflect(Exp(..), Literal(..), Prog(..))
import System.IO.Unsafe(unsafePerformIO)

-- | The NODEPTR of a value, without evaluating it: the address of its node,
-- or, for a combinator, primitive, or constructor, its encoding.
nodeAddr :: a -> Ptr Cell
nodeAddr = _primitive "nodeAddr"

primRtsInfo :: IO (Ptr ())
primRtsInfo = _primitive "IO.rtsinfo"

-- Keep a value alive until this runs.
keepAlive :: a -> IO ()
keepAlive a = nodeAddr a `seq` return ()

-----------------------------------------------------------------------------
-- The result

-- | What an entry of the extraction is.
data Why
  = Copy        -- a copied application, i.e., code
  -- Why the copy stopped at a node, and refers to it instead:
  | Whnf        -- data: a value, or a constructor with its fields
  | Redex       -- an application that would reduce
  | Cycle       -- a node that starts a cycle
  | Depth       -- the size limit, since the nodes furthest from the root are cut
  deriving (Eq, Show)

-- | The number of an entry of the extraction.  Entry 0 is the root.
type EntryNo = Int

-- | An operand of a copied application: an entry, or a leaf, i.e., a
-- combinator, primitive, constructor, FFI function, ..., which never changes.
data Opnd = Index EntryNo | Leaf (Ptr Cell) Exp
  deriving (Show)

data Entry = Entry
  { entryNode  :: Ptr Cell      -- the node it is a copy of, or refers to
  , entryWhy   :: Why
  , entryValue :: Maybe Exp     -- Whnf: the value, if it is a literal
  , entryFun   :: Opnd          -- Copy: the function,
  , entryArg   :: Opnd          --   and the argument
  }
  deriving (Show)

-- | The extracted subgraph: the entries, by their EntryNo.
newtype Extraction = Extraction (IntMap Entry)

-- | The default size limit.
defaultLimit :: Int
defaultLimit = 500

-- How many times the size limit to explore.
exploreFactor :: Int
exploreFactor = 4

-----------------------------------------------------------------------------
-- The runtime's tables, see rts_info in eval.c.

data Info = Info
  { wordBytes :: Int
  , tagNames, tagArities, caseArities, ffiTable, numFfi, xffiTable :: Ptr ()
  , dInd, dInt, dInt64, dDbl, dFlt32, dForPtr, dCCall, dBadDyn, dTick
  , vConstr, vCaseD, vLookD, vCaseS, vLookS
  , forPtrFinalizer, forPtrPayload, finalFpType, fpBstr :: Int
  }

rtsInfoVersion :: Int
rtsInfoVersion = 1

-- The unsafePerformIO is safe: rts_info is a static struct in the runtime,
-- which is filled in before any Haskell code runs, and then never changes.
-- So reading it has no side effects, and gives the same result whenever it
-- is done, and info can be an ordinary value.
-- What can change is not in info: a variable that can change, e.g.,
-- case_arities, which is reallocated when code is loaded, is given by its
-- address, and read through it each time it is used.
info :: Info
info = unsafePerformIO $ do
  p <- primRtsInfo
  let ws = sizeOf p
      ptr :: Int -> IO (Ptr ())
      ptr i = peekByteOff p (i * ws)
      int :: Int -> IO Int
      int i = (fromIntegral :: Word -> Int) <$> peekByteOff p (i * ws)
  v <- int 0
  when (v /= rtsInfoVersion) $
    error "Mhs.Extract: the runtime has another version of rts_info"
  [names, arities, cases, ffi, nffi, xffi] <- mapM ptr [2..7]
  [ind, i, i64, dbl, flt, fp, ccall, baddyn, tick,
   con, cased, lookd, cases', looks,
   fin, payload, fptype, bstr] <- mapM int [8 .. 25]
  return Info
    { wordBytes = ws
    , tagNames = names, tagArities = arities, caseArities = cases
    , ffiTable = ffi, numFfi = nffi, xffiTable = xffi
    , dInd = ind, dInt = i, dInt64 = i64, dDbl = dbl, dFlt32 = flt, dForPtr = fp
    , dCCall = ccall, dBadDyn = baddyn, dTick = tick
    , vConstr = con, vCaseD = cased, vLookD = lookd, vCaseS = cases', vLookS = looks
    , forPtrFinalizer = fin, forPtrPayload = payload, finalFpType = fptype, fpBstr = bstr
    }

tagName :: Int -> IO String
tagName t = do
  s <- peekElemOff (castPtr (tagNames info)) t
  if s == nullPtr then return ('?' : show t) else peekCString s

tagArity :: Int -> IO Int
tagArity t = fromIntegral <$> (peekElemOff (castPtr (tagArities info)) t :: IO Word8)

-- The name and arity of FFI function i, see FFI_IX.
-- An entry of the table is three words: the name, the arity, and the function.
ffiEntry :: Int -> IO (String, Int)
ffiEntry i = do
  let ws = wordBytes info
  num <- fromIntegral <$> (peek (castPtr (numFfi info)) :: IO CInt)
  e <- if i < num then
         return (ffiTable info `plusPtr` (i * 3 * ws))
       else do
         x <- peek (castPtr (xffiTable info)) :: IO (Ptr ())
         return (x `plusPtr` ((i - num) * 3 * ws))
  name <- peek (castPtr e) >>= peekCString
  arity <- peekByteOff e ws :: IO CInt
  return (name, fromIntegral arity)

-----------------------------------------------------------------------------
-- Nodes

-- | A heap cell, i.e., a node.  A Ptr Cell is a NODEPTR: the address of
-- a node, or a virtual one, i.e., an encoded tag.  See eval.c for the encoding.
data Cell

bitsOf :: Ptr Cell -> Word
bitsOf p = fromIntegral (p `minusPtr` nullPtr)

isVirt :: Ptr Cell -> Bool
isVirt n = bitsOf n .&. 3 == 2

virtTag :: Ptr Cell -> Int
virtTag n = fromIntegral ((bitsOf n `shiftR` 2) .&. 0x7ff)

-- The bits of a virtual NODEPTR above the tag.
virtInfo :: Ptr Cell -> Int
virtInfo n = fromIntegral (bitsOf n `shiftR` 13)

-- For the FUN field of a node: is it an application?
isAp :: Ptr Cell -> Bool
isAp f = bitsOf f .&. 1 == 0

-- For the FUN field of a node: the tag of a data node, or 0 (D_NONE).
dataTag :: Ptr Cell -> Int
dataTag f = if bitsOf f .&. 3 == 3 then fromIntegral (bitsOf f `shiftR` 2) else 0

peekFun :: Ptr Cell -> IO (Ptr Cell)
peekFun n = peek (castPtr n)

peekArg :: Ptr Cell -> IO (Ptr Cell)
peekArg n = peekByteOff n (wordBytes info)

-- The payload of a data node.
peekValue :: (Storable a) => Ptr Cell -> IO a
peekValue n = peekByteOff n (wordBytes info)

skipInd :: Ptr Cell -> IO (Ptr Cell)
skipInd n
  | isVirt n = return n
  | otherwise = do
      f <- peekFun n
      if dataTag f == dInd info then peekArg n >>= skipInd else return n

-- The entry of the table of case arities of a V_CASED, V_CASES, or V_LOOKS:
-- the constructor numbers of the arms (for a sparse case), and their arities.
-- An entry is three words: the number of arms, the arities, and the tags.
caseArity :: Ptr Cell -> IO (Maybe [Int], [Int])
caseArity n = do
  let ws = wordBytes info
      ix = virtInfo n .&. 0x7ffff
  tbl <- peek (castPtr (caseArities info)) :: IO (Ptr ())
  let e = tbl `plusPtr` (ix * 3 * ws)
  m <- fromIntegral <$> (peek (castPtr e) :: IO Word16)
  ap <- peekByteOff e ws :: IO (Ptr Word8)
  tp <- peekByteOff e (2 * ws) :: IO (Ptr Word16)
  as <- mapM (fmap fromIntegral . peekElemOff ap) [0 .. m - 1]
  ts <- if tp == nullPtr then return Nothing
        else Just <$> mapM (fmap fromIntegral . peekElemOff tp) [0 .. m - 1]
  return (ts, as)

-- The number of arguments the virtual node needs before it reduces,
-- or -1 if it never reduces (a constructor), see arity_virt.
arityVirt :: Ptr Cell -> IO Int
arityVirt n
  | t == vConstr info || t == vLookD info || t == vLookS info = return (-1)
  | t == vCaseD info = (+ 1) . length . snd <$> caseArity n
  | t == vCaseS info = (+ 2) . length . snd <$> caseArity n
  | otherwise = tagArity t
  where t = virtTag n

-- A leaf: a virtual node, or the node of an FFI function or a tick.
leafExp :: Ptr Cell -> IO Exp
leafExp n
  | isVirt n = virtExp
  | otherwise = do
      dt <- dataTag <$> peekFun n
      if dt == dCCall info then Lit . LFFI . fst <$> (peekValue n >>= ffiEntry)
      else if dt == dTick info then Lit . LTick . show <$> (peekValue n :: IO Int)
      else error "Mhs.Extract: not a leaf"
  where
    t = virtTag n
    k = virtInfo n .&. 0x7ff
    prim = return . Lit . LPrim
    nums = concatMap (\ i -> '_' : show i)
    virtExp
      | t == vConstr info = return (Con k ((virtInfo n `shiftR` 11) .&. 0xff))
      | t == vLookD info = prim ("L_" ++ show k)
      | t == vCaseD info = do (_, as) <- caseArity n; prim ('D' : nums as)
      | t == vCaseS info || t == vLookS info = do
          (ts, as) <- caseArity n
          prim ((if t == vCaseS info then 'E' else 'M') :
                nums (concat (zipWith (\ k' a -> [k', a]) (fromMaybe [] ts) as)))
      | otherwise = tagName t >>= prim

-- The value of a data node, if it is a literal.
valueExp :: Ptr Cell -> IO (Maybe Exp)
valueExp n = do
  t <- dataTag <$> peekFun n
  let lit l = return (Just (Lit l))
  if t == dInt info then peekValue n >>= lit . LInt
  else if t == dInt64 info then peekValue n >>= lit . LInt64
  else if t == dDbl info then peekValue n >>= lit . LDouble
  else if t == dFlt32 info then peekValue n >>= lit . LFloat
  else if t == dForPtr info then do
    fp <- peekValue n :: IO (Ptr ())
    fin <- peekByteOff fp (forPtrFinalizer info) :: IO (Ptr ())
    ty <- if fin == nullPtr then return (-1) else peekByteOff fin (finalFpType info) :: IO CInt
    if fromIntegral ty /= fpBstr info then
      return Nothing
    else do
      -- struct bytestring: the size, the capacity, and the bytes
      let pl = fp `plusPtr` forPtrPayload info
      size <- peekByteOff pl 0 :: IO Word
      arr <- peekByteOff pl (2 * wordBytes info) :: IO CString
      packCStringLen (arr, fromIntegral size) >>= lit . LBytes
  else
    return Nothing

-----------------------------------------------------------------------------
-- Extraction

-- What an application node is: code to copy, data, or a redex.
classify :: Ptr Cell -> IO Why
classify = go 0
  where
    go :: Int -> Ptr Cell -> IO Why
    go d n
      | isVirt n = do
          a <- arityVirt n
          return $ if a < 0 then Whnf else if d < a then Copy else Redex
      | otherwise = do
          f <- peekFun n
          let t = dataTag f
          if isAp f then go (d + 1) f
          else if t == dInd info then peekArg n >>= go d
          else if t == dCCall info then do
            (_, a) <- peekValue n >>= ffiEntry
            return $ if d < a then Copy else Redex
          else if t == dTick info then
            return $ if d < 1 then Copy else Redex
          else
            return $ if d == 0 then Whnf else Redex     -- a value, or an error

-- An entry while the graph is traversed.
data W = W
  { wNode   :: Ptr Cell
  , wWhy    :: Why
  , wActive :: Bool             -- being copied, i.e., on the traversal stack
  , wCycle  :: Bool             -- the traversal came back to it while it was active
  , wValue  :: Maybe Exp
  , wFun    :: Opnd
  , wArg    :: Opnd
  }

-- The state of the traversal.
data S = S
  { sEntries :: IntMap W        -- the entries, by EntryNo; the ones from sN on were dropped
  , sN       :: Int             -- the number of entries
  , sMade    :: Int             -- the number of copies made, also dropped ones
  , sNodes   :: IntMap EntryNo  -- the entry of a node, by nodeKey
  }

noOpnd :: Opnd
noOpnd = Index (-1)

-- The key of a node in sNodes.  Nodes are at least 4 byte aligned, and the
-- key must not be negative.
nodeKey :: Ptr Cell -> Int
nodeKey p = fromIntegral (bitsOf p `shiftR` 2)

setEntry :: EntryNo -> W -> S -> S
setEntry i w s = s{ sEntries = M.insert i w (sEntries s) }

newEntry :: W -> S -> (S, EntryNo)
newEntry w s =
  let i = sN s
  in  (s{ sEntries = M.insert i w (sEntries s), sN = i + 1,
          sNodes = M.insert (nodeKey (wNode w)) i (sNodes s) }, i)

-- Find the entry of a node.  The entries of dropped nodes are left behind,
-- so an entry only counts if it is still in use for the same node.
lookupNode :: Ptr Cell -> S -> Maybe EntryNo
lookupNode p s =
  case M.lookup (nodeKey p) (sNodes s) of
    Just i | i < sN s && wNode (sEntries s M.! i) == p -> Just i
    _ -> Nothing

-- Visit a node, and copy it if it is code.
visit :: Int -> S -> Ptr Cell -> IO (S, Opnd)
visit limit s p0 = do
  p <- skipInd p0
  let leaf = (,) s . Leaf p <$> leafExp p
  if isVirt p then leaf else
   case lookupNode p s of
    Just i -> do
      let w = sEntries s M.! i
      return (if wActive w && i /= 0 then setEntry i w{ wCycle = True } s else s, Index i)
    Nothing -> do
      f <- peekFun p
      mk <-
        if isAp f then
          Just <$> classify p
        else do
          let dt = dataTag f
          return $
            if dt == dCCall info || dt == dTick info then Nothing  -- a function, which never changes
            else if dt == dBadDyn info then Just Redex              -- evaluating it is an error
            else Just Whnf
      case mk of
        Nothing -> leaf
        Just k0 -> do
          let k = if k0 == Copy && sMade s >= exploreFactor * limit then Depth else k0
          v <- if k == Whnf && not (isAp f) then valueExp p else return Nothing
          let (s1, i) = newEntry (W p k (k == Copy) False v noOpnd noOpnd) s
          if k /= Copy then return (s1, Index i) else do
            (s2, fo) <- peekFun p >>= visit limit s1{ sMade = sMade s1 + 1 }
            (s3, ao) <- peekArg p >>= visit limit s2
            let w = sEntries s3 M.! i
                s4 | wCycle w  = -- Drop the copy, and everything after it.
                                 (setEntry i (W p Cycle False False Nothing noOpnd noOpnd) s3){ sN = i + 1 }
                   | otherwise = setEntry i w{ wActive = False, wFun = fo, wArg = ao } s3
            return (s4, Index i)

-- Keep the copied nodes that are nearest the root, breadth first, up to
-- the size limit.  A copied node that is used by a kept one, but is not
-- kept itself, becomes a Depth reference, and what is only reachable
-- through it is dropped.  The kept entries stay in the same order, so the
-- root stays entry 0.
select :: Int -> S -> Extraction
select limit s = Extraction (M.fromList (zip [0 ..] (map entry (M.keys used))))
  where
    es = sEntries s
    root = M.insert 0 () M.empty
    -- The entries used by the kept copies, and the kept copies.
    (used, kept) = bfs root root 1 [0] []
    -- The queue is the current list, and then the next one, which is reversed.
    bfs u kp _  [] [] = (u, kp)
    bfs u kp nk [] next = bfs u kp nk (reverse next) []
    bfs u kp nk (i : is) next =
      let w = es M.! i
          (u', kp', nk', next') = foldl step (u, kp, nk, next) [wFun w, wArg w]
      in  bfs u' kp' nk' is next'
    step acc@(u, kp, nk, next) (Index j)
      | isJust (M.lookup j u) = acc
      | wWhy (es M.! j) == Copy && nk < limit = (M.insert j () u, M.insert j () kp, nk + 1, j : next)
      | otherwise = (M.insert j () u, kp, nk, next)
    step acc _ = acc
    newIndex = M.fromList (zip (M.keys used) [0 ..])
    re (Index j) = Index (newIndex M.! j)
    re l = l
    entry i =
      let w = es M.! i in
      case wWhy w of
        Copy | isJust (M.lookup i kept) -> Entry (wNode w) Copy Nothing (re (wFun w)) (re (wArg w))
             | otherwise                -> Entry (wNode w) Depth Nothing noOpnd noOpnd
        why                             -> Entry (wNode w) why (wValue w) noOpnd noOpnd

-- | Extract the code of a function, i.e., a partial application, with at most
-- limit copied nodes.  The value is evaluated first.  Nothing if it is not
-- a partial application.
extract :: Int -> a -> IO (Maybe Extraction)
extract limit a = a `seq` do
  r <- skipInd (nodeAddr a)
  code <- if isVirt r then return False else do
            f <- peekFun r
            if isAp f then (== Copy) <$> classify r else return False
  res <- if not code then return Nothing else do
    (s, _) <- visit limit (S M.empty 0 0 M.empty) r
    return (Just (select limit s))
  keepAlive a
  return res

-----------------------------------------------------------------------------
-- Using the result

-- The number of uses of each entry by the copies.
uses :: Extraction -> EntryNo -> Int
uses (Extraction es) = \ i -> fromMaybe 0 (M.lookup i us)
  where us = foldr (\ j -> M.insertWith (+) j 1) M.empty
               [ j | (_, e) <- M.toList es, entryWhy e == Copy, Index j <- [entryFun e, entryArg e] ]

-- | The extracted code as combinator code, for Mhs.Reflect.decompile.
-- The root is self when the code refers to it.  A copied node that is used
-- more than once is a definition, _i, where i is its entry.  A reference
-- is a variable, e.g., redex_i, except for a literal value.
extractionProg :: Extraction -> Prog
extractionProg x@(Extraction es) = Prog defs body
  where
    us = uses x
    shared i = i /= 0 && entryWhy (es M.! i) == Copy && us i > 1
    name 0 = "self"
    name i = case entryWhy (es M.! i) of
               Copy -> '_' : show i
               why  -> map toLower (show why) ++ "_" ++ show i
    opnd (Leaf _ e) = e
    opnd (Index i) | i == 0 || shared i = Var (name i)
                   | otherwise = entryExp i
    entryExp i =
      let e = es M.! i in
      case entryWhy e of
        Copy -> App (opnd (entryFun e)) (opnd (entryArg e))
        _    -> fromMaybe (Var (name i)) (entryValue e)
    recursive = us 0 > 0
    defs = [ (name 0, entryExp 0) | recursive ] ++
           [ (name i, entryExp i) | i <- M.keys es, shared i ]
    body = if recursive then Var (name 0) else entryExp 0

-- | The number of entries of each kind.
extractionSummary :: Extraction -> String
extractionSummary x@(Extraction es) =
  show (count Copy) ++ " nodes copied, " ++ show (uses x 0) ++
  " references back to the root, references that stopped the copy: whnf " ++
  show (count Whnf) ++ ", redex " ++ show (count Redex) ++ ", cycle " ++
  show (count Cycle) ++ ", depth " ++ show (count Depth)
  where count w = length [ () | (_, e) <- M.toList es, entryWhy e == w ]
