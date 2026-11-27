-- Copyright 2026 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Reading the graph, and extracting the code of a function: the part of the
-- graph of its value that is code, i.e., the partial applications that make
-- up the function.
-- The graph is read directly: nodeAddr gives the NODEPTR of a value,
-- decodeNodePtr says what it is, and a cell is read with peek.  See NODEPTR
-- and struct node in eval.c for the encoding, and rts_info for the tables
-- that describe it.
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
  -- The graph
  nodeAddr, NodePtr(..), Comb(..), Cell(..), decodeNodePtr, encodeNodePtr,
  -- Extraction
  Extraction(..), EntryNo, Entry(..), Why(..), Opnd(..),
  defaultLimit, extract, extractionProg, extractionSummary,
  ) where
import Control.Monad
import Data.Bits
import Data.ByteString(packCStringLen)
import Data.Char(toLower)
import Data.Int(Int64)
import Data.Maybe(fromMaybe, isJust, isNothing)
import Data.Word(Word8, Word16)
import Foreign.C.String(CString, peekCString)
import Foreign.C.Types(CInt)
import Foreign.Ptr
import Foreign.Storable
import Mhs.IntMap(IntMap)
import qualified Mhs.IntMap as M
import Mhs.Reflect(Exp(..), Literal(..), Prog(..))
import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------
-- The graph

-- | The NODEPTR of a value, without evaluating it, see decodeNodePtr.
nodeAddr :: a -> Int
nodeAddr = _primitive "nodeAddr"

primRtsInfo :: IO (Ptr ())
primRtsInfo = _primitive "IO.rtsinfo"

-- Keep a value alive until this runs.
keepAlive :: a -> IO ()
keepAlive a = nodeAddr a `seq` return ()

-- | What a NODEPTR is.  Its two lowest bits tell.
data NodePtr
  = CellPtr (Ptr Cell)  -- ...00: a pointer to a cell
  | Comb Comb           -- ...10: a combinator, primitive, constructor, or case,
                        --   which is encoded in the NODEPTR instead of a cell
  | ValueTag Int        -- ...11: only in the function field of a cell, which
                        --   then holds a value instead of an application;
                        --   the tag says what the value is, e.g., D_INT
  deriving (Eq, Show)

-- | A combinator, primitive, constructor, or case.  The constructors and
-- cases are families, e.g., C_k_n, so they are indexed.
data Comb
  = Prim Int            -- a combinator or primitive, e.g., S or +: its tag
  | C Int Int           -- C_k_n: constructor number k, with n fields
  | D Int               -- D_a0_..._am: a dense case: its index in the table of case arities
  | E Int               -- E_k0_a0_..._km_am: a sparse case: ditto
  deriving (Eq, Show)

-- | A heap cell, i.e., a node: an application, or a value.  The values are
-- named after their tags, e.g., D_INT.
data Cell
  = AP NodePtr NodePtr  -- the function and the argument
  | IND NodePtr         -- an indirection
  | INT Int
  | INT64 Int64
  | DBL Double
  | FLT32 Float
  | PTR (Ptr ())
  | FUNPTR (FunPtr ())
  | FORPTR (Ptr ())     -- a ForeignPtr or a ByteString: a struct forptr
  | ARR (Ptr ())        -- an array: a struct ioarray
  | THID (Ptr ())       -- a thread: a struct mthread
  | MVAR (Ptr ())       -- a struct mvar
  | WEAK (Ptr ())       -- a struct weak_ptr
  | IO_CCALL Int        -- an FFI function: its number, see FFI_IX
  | BADDYN CString      -- an unknown FFI function: its name
  | TICK Int            -- a tick: its number in the tick table
  | DATA Int Word       -- any other value: its tag, and the payload
  deriving (Eq, Show)

-- | What a NODEPTR is.
decodeNodePtr :: Int -> NodePtr
decodeNodePtr w =
  case w .&. 3 of
    0 -> CellPtr (nullPtr `plusPtr` w)
    2 -> Comb (virt ((w `shiftR` 2) .&. 0x7ff) (w `shiftR` 13))
    3 -> ValueTag (w `shiftR` 2)
    _ -> error ("Mhs.Extract.decodeNodePtr: not a NODEPTR: " ++ show w)
  where
    virt t x
      | t == vConstr info = C (x .&. 0x7ff) ((x `shiftR` 11) .&. 0xff)
      | t == vCaseD info  = D (x .&. 0x7ffff)
      | t == vCaseS info  = E (x .&. 0x7ffff)
      | otherwise         = Prim t

-- | The NODEPTR of a NodePtr.
encodeNodePtr :: NodePtr -> Int
encodeNodePtr (CellPtr p) = p `minusPtr` nullPtr
encodeNodePtr (ValueTag t) = t `shiftL` 2 .|. 3
encodeNodePtr (Comb c) =
  case c of
    Prim t -> virt t 0
    C k n  -> virt (vConstr info) (n `shiftL` 11 .|. k)
    D i    -> virt (vCaseD info) i
    E i    -> virt (vCaseS info) i
  where virt t x = (x `shiftL` 11 .|. t) `shiftL` 2 .|. 2

-- A cell is two NODEPTRs, the function and the argument, or a value tag and
-- the payload, which can be wider than a NODEPTR.
instance Storable Cell where
  sizeOf _ = nodeSize info
  alignment _ = wordBytes info
  peek p = do
    f <- field 0
    case f of
      ValueTag t -> value t
      _          -> AP f <$> field (payload info)
    where
      field o = decodeNodePtr <$> peekByteOff p o
      val :: forall a . Storable a => IO a
      val = peekByteOff p (payload info)
      value t
        | t == dInd info     = IND <$> field (payload info)
        | t == dInt info     = INT <$> val
        | t == dInt64 info   = INT64 <$> val
        | t == dDbl info     = DBL <$> val
        | t == dFlt32 info   = FLT32 <$> val
        | t == dPtr info     = PTR <$> val
        | t == dFunPtr info  = FUNPTR <$> val
        | t == dForPtr info  = FORPTR <$> val
        | t == dArr info     = ARR <$> val
        | t == dThid info    = THID <$> val
        | t == dMVar info    = MVAR <$> val
        | t == dWeak info    = WEAK <$> val
        | t == dCCall info   = IO_CCALL <$> val
        | t == dBadDyn info  = BADDYN <$> val
        | t == dTick info    = TICK <$> val
        | otherwise          = DATA t <$> val
  poke p c =
    case c of
      AP f a     -> field 0 f >> field (payload info) a
      IND a      -> tag dInd >> field (payload info) a
      INT x      -> tag dInt >> val x
      INT64 x    -> tag dInt64 >> val x
      DBL x      -> tag dDbl >> val x
      FLT32 x    -> tag dFlt32 >> val x
      PTR x      -> tag dPtr >> val x
      FUNPTR x   -> tag dFunPtr >> val x
      FORPTR x   -> tag dForPtr >> val x
      ARR x      -> tag dArr >> val x
      THID x     -> tag dThid >> val x
      MVAR x     -> tag dMVar >> val x
      WEAK x     -> tag dWeak >> val x
      IO_CCALL x -> tag dCCall >> val x
      BADDYN x   -> tag dBadDyn >> val x
      TICK x     -> tag dTick >> val x
      DATA t x   -> field 0 (ValueTag t) >> val x
    where
      field o n = pokeByteOff p o (encodeNodePtr n)
      tag d = field 0 (ValueTag (d info))
      val :: forall a . Storable a => a -> IO ()
      val = pokeByteOff p (payload info)

-----------------------------------------------------------------------------
-- The runtime's tables, see rts_info in eval.c.

data Info = Info
  { wordBytes, nodeSize, payload :: Int
  , tagNames, tagArities, caseArities, ffiTable, numFfi, xffiTable :: Ptr ()
  , dInd, dInt, dInt64, dDbl, dFlt32, dPtr, dFunPtr, dForPtr
  , dArr, dThid, dMVar, dWeak, dCCall, dBadDyn, dTick
  , vConstr, vCaseD, vCaseS
  , forPtrFinalizer, forPtrPayload, finalFpType, fpBstr :: Int
  }

rtsInfoVersion :: Int
rtsInfoVersion = 2

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
  [size, pl] <- mapM int [1, 2]
  [names, arities, cases, ffi, nffi, xffi] <- mapM ptr [4 .. 9]
  [ind, i, i64, dbl, flt, ptr', funptr, fp, arr, thid, mvar, weak, ccall, baddyn, tick,
   con, cased, cases',
   fin, fppayload, fptype, bstr] <- mapM int [10 .. 31]
  return Info
    { wordBytes = ws, nodeSize = size, payload = pl
    , tagNames = names, tagArities = arities, caseArities = cases
    , ffiTable = ffi, numFfi = nffi, xffiTable = xffi
    , dInd = ind, dInt = i, dInt64 = i64, dDbl = dbl, dFlt32 = flt, dPtr = ptr'
    , dFunPtr = funptr, dForPtr = fp, dArr = arr, dThid = thid, dMVar = mvar, dWeak = weak
    , dCCall = ccall, dBadDyn = baddyn, dTick = tick
    , vConstr = con, vCaseD = cased, vCaseS = cases'
    , forPtrFinalizer = fin, forPtrPayload = fppayload, finalFpType = fptype, fpBstr = bstr
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

-- Entry ix of the table of case arities: the constructor numbers of the
-- arms (for a sparse case), and their arities.
-- An entry is three words: the number of arms, the arities, and the tags.
caseArity :: Int -> IO (Maybe [Int], [Int])
caseArity ix = do
  let ws = wordBytes info
  tbl <- peek (castPtr (caseArities info)) :: IO (Ptr ())
  let e = tbl `plusPtr` (ix * 3 * ws)
  m <- fromIntegral <$> (peek (castPtr e) :: IO Word16)
  ap <- peekByteOff e ws :: IO (Ptr Word8)
  tp <- peekByteOff e (2 * ws) :: IO (Ptr Word16)
  as <- mapM (fmap fromIntegral . peekElemOff ap) [0 .. m - 1]
  ts <- if tp == nullPtr then return Nothing
        else Just <$> mapM (fmap fromIntegral . peekElemOff tp) [0 .. m - 1]
  return (ts, as)

-- The number of arguments a combinator needs before it reduces,
-- or -1 if it never reduces (a constructor), see arity_virt.
combArity :: Comb -> IO Int
combArity c =
  case c of
    Prim t -> tagArity t
    C _ _  -> return (-1)
    D i    -> (+ 1) . length . snd <$> caseArity i
    E i    -> (+ 2) . length . snd <$> caseArity i

combExp :: Comb -> IO Exp
combExp c =
  case c of
    Prim t -> tagName t >>= prim
    C k n  -> return (Con k n)
    D i    -> do (_, as) <- caseArity i; prim ('D' : nums as)
    E i    -> do (ts, as) <- caseArity i
                 prim ('E' : nums (concat (zipWith (\ k a -> [k, a]) (fromMaybe [] ts) as)))
  where
    prim = return . Lit . LPrim
    nums = concatMap (\ i -> '_' : show i)

-- The value of a cell, if it is a literal.
valueExp :: Cell -> IO (Maybe Exp)
valueExp c =
  case c of
    INT x     -> lit (LInt x)
    INT64 x   -> lit (LInt64 x)
    DBL x     -> lit (LDouble x)
    FLT32 x   -> lit (LFloat x)
    FORPTR fp -> do
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
    _ -> return Nothing
  where lit = return . Just . Lit

skipInd :: NodePtr -> IO NodePtr
skipInd n@(CellPtr p) = do
  c <- peek p
  case c of
    IND a -> skipInd a
    _     -> return n
skipInd n = return n

-- The function and the argument of an application.
apFields :: Ptr Cell -> IO (NodePtr, NodePtr)
apFields p = do
  c <- peek p
  case c of
    AP f a -> return (f, a)
    _      -> error "Mhs.Extract: not an application"

notANode :: NodePtr -> a
notANode n = error ("Mhs.Extract: not a node: " ++ show n)

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
data Opnd = Index EntryNo | Leaf NodePtr Exp
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
-- Extraction

-- What an application is: code to copy, data, or a redex.  The arguments
-- on the spine are counted, and compared with the arity of the head.
classify :: NodePtr -> IO Why
classify = go 0
  where
    go :: Int -> NodePtr -> IO Why
    go d (Comb c) = do
      a <- combArity c
      return $ if a < 0 then Whnf else spine d a
    go d (CellPtr p) = do
      c <- peek p
      case c of
        AP f _     -> go (d + 1) f
        IND a      -> go d a
        IO_CCALL i -> spine d . snd <$> ffiEntry i
        TICK _     -> return (spine d 1)
        _          -> return (if d == 0 then Whnf else Redex)  -- a value, or an error
    go _ n = notANode n
    spine d a = if d < a then Copy else Redex

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
nodeKey p = fromIntegral ((fromIntegral (p `minusPtr` nullPtr) :: Word) `shiftR` 2)

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
visit :: Int -> S -> NodePtr -> IO (S, Opnd)
visit limit s n0 = do
  n <- skipInd n0
  let leaf e = (s, Leaf n e)
  case n of
    Comb c -> leaf <$> combExp c
    CellPtr p | Just i <- lookupNode p s -> do
      let w = sEntries s M.! i
      return (if wActive w && i /= 0 then setEntry i w{ wCycle = True } s else s, Index i)
    CellPtr p -> do
      c <- peek p
      case c of
        -- A function, which never changes.
        IO_CCALL i -> leaf . Lit . LFFI . fst <$> ffiEntry i
        TICK i     -> return (leaf (Lit (LTick (show i))))
        _ -> do
          k0 <- case c of
                  AP _ _   -> classify n
                  BADDYN _ -> return Redex      -- evaluating it is an error
                  _        -> return Whnf
          let k = if k0 == Copy && sMade s >= exploreFactor * limit then Depth else k0
          v <- if k == Whnf then valueExp c else return Nothing
          let (s1, i) = newEntry (W p k (k == Copy) False v noOpnd noOpnd) s
          if k /= Copy then return (s1, Index i) else do
            -- Each field is read again, just before it is visited, see above.
            (s2, fo) <- fst <$> apFields p >>= visit limit s1{ sMade = sMade s1 + 1 }
            (s3, ao) <- snd <$> apFields p >>= visit limit s2
            let w = sEntries s3 M.! i
                s4 | wCycle w  = -- Drop the copy, and everything after it.
                                 (setEntry i (W p Cycle False False Nothing noOpnd noOpnd) s3){ sN = i + 1 }
                   | otherwise = setEntry i w{ wActive = False, wFun = fo, wArg = ao } s3
            return (s4, Index i)
    _ -> notANode n

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
  r <- skipInd (decodeNodePtr (nodeAddr a))
  code <- case r of
            CellPtr p -> do
              c <- peek p
              case c of
                AP _ _ -> (== Copy) <$> classify r
                _      -> return False
            _ -> return False
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

-- | The extracted code as combinator code, for Mhs.Reflect.decompile, and
-- the cells that its free variables refer to.
-- The root is self when the code refers to it.  A copied node that is used
-- more than once is a definition, _i, where i is its entry.  A reference
-- is a free variable, e.g., redex_i, except for a literal value.
-- The cells are only valid as long as the extracted value is alive, and
-- a redex changes when it is evaluated.
extractionProg :: Extraction -> (Prog, [(String, Ptr Cell)])
extractionProg x@(Extraction es) = (Prog defs body, refs)
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
    refs = [ (name i, entryNode e) | (i, e) <- M.toList es,
                                     entryWhy e /= Copy, isNothing (entryValue e) ]

-- | The number of entries of each kind.
extractionSummary :: Extraction -> String
extractionSummary x@(Extraction es) =
  show (count Copy) ++ " nodes copied, " ++ show (uses x 0) ++
  " references back to the root, references that stopped the copy: whnf " ++
  show (count Whnf) ++ ", redex " ++ show (count Redex) ++ ", cycle " ++
  show (count Cycle) ++ ", depth " ++ show (count Depth)
  where count w = length [ () | (_, e) <- M.toList es, entryWhy e == w ]
