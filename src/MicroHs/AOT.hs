-- Copyright 2026 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Experimental: translate some definitions to C, ahead of time, to see what
-- compiled code could gain (see jit-plan.md, and aot_entries in eval.c).
--
-- The definitions to translate are given with --aot=NAME,...  They are not
-- bracket abstracted (aotMark), and when linking, aotCompile turns each into
--   aot:i [c1 ... cn]
-- an entry of the compiled code applied to an array of the constants the code
-- uses.  The constants are the closed, lambda free, subexpressions, so they
-- are shared between calls, as with bracket abstraction (full laziness).
--
-- The code for an entry gets its arguments from the stack, and returns the
-- next node to evaluate.  Code in tail position overwrites the redex.  To
-- evaluate something (a case scrutinee, an Int) the code first checks if it
-- is already evaluated, and if so goes on with a "piece" (a C function) for
-- the rest of the code.  Otherwise it pushes the variables the piece needs and
-- a continuation marker, and returns the node to evaluate; when it has been
-- evaluated, the runtime calls the continuation, which pops them and calls
-- the piece.  The code has no loops and never calls evali, so the number of
-- cells it allocates before it returns is bounded, and the runtime checks for
-- them before it calls the code.  Lambdas, and lazy expressions with control
-- (case, let), become entries of their own, applied to their free variables.
module MicroHs.AOT(aotMark, aotSelect, aotCompile) where
import qualified Prelude(); import MHSPrelude
import Data.Char(isAlphaNum)
import Data.List
import Data.Maybe
import qualified MicroHs.IdentMap as M
import Control.Monad(foldM)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), PrimOp(..))
import MicroHs.Ident
import MicroHs.State

-- Mark a definition for translation.
aotMark :: Exp -> Exp
aotMark = App (litPrim "aot$def")

-- Should a definition be compiled?  The names are the arguments of --aot,
-- where * means all functions, and M.* the functions of module M.
aotSelect :: [String] -> Ident -> Exp -> Bool
aotSelect [] _ _ = False
aotSelect ns i e = s `elem` ns || (hasLam e && isNothing (getForExp e) && any match ns)
  where s = showIdent i
        match "*" = True
        match n | Just m <- stripSuffix ".*" n = (m ++ ".") `isPrefixOf` s && notElem '.' (drop (length m + 1) s)
        match _ = False
        stripSuffix suf str = fmap reverse (stripPrefix (reverse suf) (reverse str))

getMarked :: Exp -> Maybe Exp
getMarked (App (Lit (LPrim (POther "aot$def"))) e) = Just e
getMarked _ = Nothing

-- Translate the marked definitions.  Returns the definitions, and the C code,
-- which is empty if there are none.
aotCompile :: [(Ident, Exp)] -> ([(Ident, Exp)], String)
aotCompile ds
  | not (any (isJust . getMarked . snd) ds) = (ds, "")
  | otherwise =
    let (ds', s) = runState (mapM def ds) gs0
        def (i, e) | Just e' <- getMarked e = do { r <- cDef i e'; return (i, r) }
                   | otherwise = return (i, e)
    in  (ds', cFile s)

------------------------------------------------------------------------------

type CVar = String

-- What a variable is in C: a node, an evaluated Int, or both.
data Val = VNode CVar | VInt CVar | VBoth CVar CVar

type Env = M.Map Val

-- C variables of a Val, and their types
cvarsOf :: Val -> [(CVar, String)]
cvarsOf (VNode n) = [(n, "NODEPTR")]
cvarsOf (VInt i) = [(i, "value_t")]
cvarsOf (VBoth n i) = [(n, "NODEPTR"), (i, "value_t")]

-- The array of constants, and the redex, are in the environment too.
kId, rId :: Ident
kId = mkIdent "aot$K"
rId = mkIdent "aot$R"

-- Code, with the number of cells it allocates.  Every path ends in a return.
data Code
  = CS Int String               -- statement(s), allocating n cells
  | CSeq [Code]
  | CIf String Code Code
  | CSwitch String [(Int, Code)] Code
  | CRet String                 -- return a node
  | CCall String [String]       -- return the result of a piece

data GS = GS
  { gsNext    :: Int
  , gsFuns    :: [String]                  -- C functions, reversed
  , gsProtos  :: [String]
  , gsBounds  :: [(String, Int)]           -- the allocation bound of each piece
  , gsEntries :: [(Int, Int, Int, String, String)]  -- index, arity, bound, function, name
  , gsNEntry  :: Int
  , gsConts   :: [(Int, Int, String)]      -- index, bound, function
  , gsNCont   :: Int
  , gsConsts  :: [Exp]                     -- constants of the current definition, reversed
  , gsUses    :: [CVar]                    -- C variables used by the current piece
  , gsDef     :: String                    -- the definition being compiled
  }

gs0 :: GS
gs0 = GS 0 [] [] [] [] 0 [] 0 [] [] ""

type G a = State GS a

fresh :: String -> G CVar
fresh p = do
  s <- get
  put s{ gsNext = gsNext s + 1 }
  return (p ++ show (gsNext s))

use :: CVar -> G ()
use c = modify $ \ s -> if c `elem` gsUses s then s else s{ gsUses = c : gsUses s }

-- Look up a local variable, and note its use.
lookupVar :: Env -> Ident -> G (Maybe Val)
lookupVar env x =
  case M.lookup x env of
    Nothing -> return Nothing
    Just v -> do
      case v of
        VNode n -> use n
        VInt i -> use i
        VBoth _ _ -> return ()   -- the user notes which part it uses
      return (Just v)

-- Note the use of the C variables in a C expression that was made in an
-- earlier piece.
useExpr :: String -> G ()
useExpr = mapM_ use . words . map (\ c -> if isAlphaNum c || c == '_' then c else ' ')

isLocal :: Env -> Ident -> Bool
isLocal env x = isJust (M.lookup x env)

getNode :: Env -> Ident -> G CVar
getNode env x = do
  mv <- lookupVar env x
  case mv of
    Just (VNode n) -> return n
    Just (VBoth n _) -> do { use n; return n }
    _ -> error $ "aot getNode " ++ showIdent x

redex :: Env -> G CVar
redex env = getNode env rId

-- The array of constants, and constant i in it.
constant :: Env -> Exp -> G String
constant env e = do
  k <- getNode env kId
  s <- get
  let cs = reverse (gsConsts s)
  i <- case elemIndex e cs of
         Just i -> return i
         Nothing -> do
           put s{ gsConsts = e : gsConsts s }
           return (length cs)
  return $ "AOTK(" ++ k ++ ", " ++ show i ++ ")"

emitFun :: String -> [String] -> Code -> G ()
emitFun hdr pre code = modify $ \ s ->
  s{ gsFuns = (unlines ([hdr, "{"] ++ map ("  " ++) pre ++ render 1 code ++ ["}"])) : gsFuns s
   , gsProtos = (hdr ++ ";") : gsProtos s }

allocBound :: Code -> G Int
allocBound code = do
  bs <- gets gsBounds
  let b (CS n _) = n
      b (CSeq cs) = sum (map b cs)
      b (CIf _ c1 c2) = max (b c1) (b c2)
      b (CSwitch _ alts d) = maximum (b d : map (b . snd) alts)
      b (CRet _) = 0
      b (CCall f _) = fromMaybe (error "aot allocBound") (lookup f bs)
  return (b code)

render :: Int -> Code -> [String]
render ind code =
  case code of
    CS _ s -> map (sp ++) (lines s)
    CSeq cs -> concatMap (render ind) cs
    CIf c t e -> [sp ++ "if (" ++ c ++ ") {"] ++ render (ind + 1) t ++ [sp ++ "} else {"] ++ render (ind + 1) e ++ [sp ++ "}"]
    CSwitch v alts d ->
      [sp ++ "switch (" ++ v ++ ") {"] ++
      concat [ [sp ++ "case " ++ show k ++ ": {"] ++ render (ind + 1) c ++ [sp ++ "}"] | (k, c) <- alts ] ++
      [sp ++ "default: {"] ++ render (ind + 1) d ++ [sp ++ "}", sp ++ "}"]
    CRet s -> [sp ++ "return " ++ s ++ ";"]
    CCall f as -> [sp ++ "return " ++ f ++ "(" ++ intercalate ", " as ++ ");"]
  where sp = replicate (2 * ind) ' '

------------------------------------------------------------------------------

-- The methods of the Int instances are primitives (see lib/Data/Int/Int.hs).
intMethods :: [((String, String), String)]
intMethods =
  [ (("Data.Num.+", num), "+"), (("Data.Num.-", num), "-"), (("Data.Num.*", num), "*")
  , (("Data.Eq.==", eq), "=="), (("Data.Eq./=", eq), "/=")
  , (("Data.Ord.<", ord), "<"), (("Data.Ord.<=", ord), "<="), (("Data.Ord.>", ord), ">"), (("Data.Ord.>=", ord), ">=")
  , (("Data.Ord.compare", ord), "icmp")
  ]
  where num = "inst$Data.Num.Num@Primitives.Int"
        eq  = "inst$Data.Eq.Eq@Primitives.Int"
        ord = "inst$Data.Ord.Ord@Primitives.Int"

resolveMethods :: Exp -> Exp
resolveMethods ae =
  case ae of
    App (Var m) (Var d) | Just p <- lookup (showIdent m, showIdent d) intMethods -> litPrim p
    App f a -> App (resolveMethods f) (resolveMethods a)
    Lam x e -> Lam x (resolveMethods e)
    _ -> ae

arithOps :: [(String, String)]
arithOps = [("+", "AOT_ADD"), ("-", "AOT_SUB"), ("*", "AOT_MUL")]

cmpOps :: [(String, String)]
cmpOps = [("==", "=="), ("/=", "!="), ("<", "<"), ("<=", "<="), (">", ">"), (">=", ">=")]

-- A binary Int primitive
getBinOp :: Exp -> Maybe (String, Exp, Exp)
getBinOp (App (App (Lit (LPrim (POther op))) a) b) | isJust (lookup op arithOps) || isJust (lookup op cmpOps) || op == "icmp" = Just (op, a, b)
getBinOp _ = Nothing

-- The C expression for an Int, if it is known without evaluation.
knownInt :: Env -> Exp -> G (Maybe String)
knownInt _ (Lit (LInt i)) = return $ Just $ cInt i
knownInt env (Var x) =
  case M.lookup x env of
    Just (VInt i) -> do { use i; return (Just i) }
    Just (VBoth _ i) -> do { use i; return (Just i) }
    _ -> return Nothing
knownInt env e | Just (op, a, b) <- getBinOp e, Just f <- lookup op arithOps = do
  ma <- knownInt env a
  mb <- knownInt env b
  return $ do { x <- ma; y <- mb; return $ f ++ "(" ++ x ++ ", " ++ y ++ ")" }
knownInt _ _ = return Nothing

cInt :: Int -> String
cInt i = "((value_t)" ++ show i ++ ")"

-- A case: the arms and the scrutinee.
data Alts = Dense [Int] [Exp] | Sparse Exp [(Int, Int, Exp)]

getCase :: Exp -> Maybe (Alts, Exp)
getCase ae =
  case spine ae [] of
    (Lit (LPrim (PCaseD as)), es) | length es == length as + 1 -> Just (Dense as (init es), last es)
    (Lit (LPrim (PCaseS m)), d : es) | length es == 2 * m + 1 ->
      let arms (Lit (LPrim (PArm k n)) : f : r) = (k, n, f) : arms r
          arms _ = []
      in  Just (Sparse d (arms (init es)), last es)
    _ -> Nothing
  where spine (App f a) as = spine f (a : as)
        spine f as = (f, as)

hasLam :: Exp -> Bool
hasLam (Lam _ _) = True
hasLam (App f a) = hasLam f || hasLam a
hasLam _ = False

-- No local variables
noFree :: Env -> Exp -> Bool
noFree env e = not (any (isLocal env) (freeVars e))

-- No local variables, and no lambdas
closed :: Env -> Exp -> Bool
closed env e = noFree env e && not (hasLam e)

isLet :: Exp -> Bool
isLet (App (Lam _ _) _) = True
isLet _ = False

-- Turn an expression without local variables into one without lambdas, by
-- making the lambdas, and the expressions with control (case, let), entries
-- of their own.  The result is a constant, shared between calls, as with
-- bracket abstraction.
closedify :: Exp -> G Exp
closedify ae
  | not (hasLam ae) = return ae
  | otherwise =
    case ae of
      Lam _ _ -> do { d <- gets gsDef; let { (xs, b) = getLams ae }; genIndependent (d ++ "/closed") xs b }
      _ | isLet ae || isJust (getCase ae) -> do { d <- gets gsDef; genIndependent (d ++ "/closed") [] ae }
      App f a -> do { f' <- closedify f; a' <- closedify a; return (App f' a') }
      _ -> return ae

-- An entry for a function without free variables, with an array of its own:
--   aot:j [c1 ... cn]
genIndependent :: String -> [Ident] -> Exp -> G Exp
genIndependent name xs body = do
  saved <- gets gsConsts
  modify $ \ s -> s{ gsConsts = [] }
  j <- genEntry name [] xs body
  cs <- gets (reverse . gsConsts)
  modify $ \ s -> s{ gsConsts = saved }
  return $ App (litPrim ("aot:" ++ show j)) (apps (litPrim "aot$arr") cs)

------------------------------------------------------------------------------

cDef :: Ident -> Exp -> G Exp
cDef i e = do
  modify $ \ s -> s{ gsDef = showIdent i }
  let (xs, body) = getLams (fullLazy (inlineAtoms (resolveMethods e)))
  genIndependent (showIdent i) xs body

-- Inline the lets that bind a variable or a literal, e.g., a dictionary.
inlineAtoms :: Exp -> Exp
inlineAtoms ae =
  case ae of
    App (Lam x b) e | atomic e -> inlineAtoms (substExp x e b)
    App f a -> App (inlineAtoms f) (inlineAtoms a)
    Lam x b -> Lam x (inlineAtoms b)
    _ -> ae
  where atomic (Var _) = True
        atomic (Lit (LInt _)) = True
        atomic _ = False

-- Full laziness, as bracket abstraction has it: a subexpression of the
-- body of a lambda that does not depend on the lambda's variable is
-- evaluated at most once per evaluation of the lambda.  So the non-trivial
-- subexpressions that depend on local variables, but not on the variable,
-- are bound by a let outside the lambda.  (Those that only depend on globals
-- are constants.)  Case arms and let bindings are applied once, so nothing
-- is floated out of them.  An entry gets the lambdas up to the first let,
-- so the code for the rest is a closure, as with bracket abstraction.
fullLazy :: Exp -> Exp
fullLazy ae = evalState (flE [] ae) (0 :: Int)

flE :: [Ident] -> Exp -> State Int Exp
flE sc ae =
  case ae of
    App (Lam x b) e -> do
      b' <- flE (x : sc) b
      e' <- flE sc e
      return $ App (Lam x b') e'
    _ | Just (alts, scrut) <- getCase ae -> do
      let arm n f = flArm n sc f
      scrut' <- flE sc scrut
      case alts of
        Dense as fs -> do
          fs' <- sequence (zipWith arm as fs)
          return $ apps (Lit (LPrim (PCaseD as))) (fs' ++ [scrut'])
        Sparse d afs -> do
          d' <- flE sc d
          afs' <- mapM (\ (k, n, f) -> do { f' <- arm n f; return [Lit (LPrim (PArm k n)), f'] }) afs
          return $ apps (Lit (LPrim (PCaseS (length afs)))) ([d'] ++ concat afs' ++ [scrut'])
    Lam x b -> do
      b' <- flE (x : sc) b
      (b'', binds) <- extract sc [x] b' []
      return $ foldl (\ acc (m, e) -> App (Lam m acc) e) (Lam x b'') binds
    App f a -> do
      f' <- flE sc f
      a' <- flE sc a
      return $ App f' a'
    _ -> return ae

-- An arm with n fields: the first n lambdas bind the fields.
flArm :: Int -> [Ident] -> Exp -> State Int Exp
flArm n sc (Lam x b) | n > 0 = do { b' <- flArm (n - 1) (x : sc) b; return (Lam x b') }
flArm _ sc e = flE sc e

-- Replace the maximal subexpressions to float by variables.  bound are the
-- variables bound inside the lambda, including its own.  Returns the new
-- expression and the bindings, innermost first.
extract :: [Ident] -> [Ident] -> Exp -> [(Ident, Exp)] -> State Int (Exp, [(Ident, Exp)])
extract sc bound ae binds
  | nonTrivial ae, not (any (`elem` bound) fvs), any (`elem` sc) fvs = do
    n <- get
    put (n + 1)
    let m = mkIdent ("aot$m" ++ show n)
    return (Var m, binds ++ [(m, ae)])
  | otherwise =
    case ae of
      App f a -> do
        (f', binds1) <- extract sc bound f binds
        (a', binds2) <- extract sc bound a binds1
        return (App f' a', binds2)
      Lam y b -> do
        (b', binds1) <- extract sc (y : bound) b binds
        return (Lam y b', binds1)
      _ -> return (ae, binds)
  where fvs = freeVars ae

-- Is evaluating an expression work?  Not for a constructor application.
nonTrivial :: Exp -> Bool
nonTrivial ae =
  case ae of
    App f _ -> not (isCon (spineHead f))
    _ -> False
  where spineHead (App f _) = spineHead f
        spineHead e = e
        isCon (Lit (LPrim (PCon _ _))) = True
        isCon _ = False

getLams :: Exp -> ([Ident], Exp)
getLams (Lam x e) = let (xs, b) = getLams e in (x : xs, b)
getLams e = ([], e)

-- An entry for the function \ fvs xs -> body, applied to the array.
genEntry :: String -> [Ident] -> [Ident] -> Exp -> G Int
genEntry name fvs xs body = do
  j <- gets gsNEntry
  modify $ \ s -> s{ gsNEntry = j + 1 }
  vk <- fresh "k"
  vfs <- mapM (const (fresh "f")) fvs
  vxs <- mapM (const (fresh "a")) xs
  vr <- fresh "r"
  let arity = 1 + length fvs + length xs
      -- A later binding of a name shadows an earlier one (M.fromList keeps the first).
      env = foldl (\ m (x, v) -> M.insert x v m) M.empty $
              (kId, VNode vk) : (rId, VNode vr) : zip fvs (map VNode vfs) ++ zip xs (map VNode vxs)
  saved <- gets gsUses
  modify $ \ s -> s{ gsUses = [] }
  code <- cTail' env body
  modify $ \ s -> s{ gsUses = saved }
  bound <- allocBound code
  let fn = "aot_e_" ++ show j
      pre = ("NODEPTR " ++ vk ++ " = ARG(TOP(0));") :
            [ "NODEPTR " ++ v ++ " = ARG(TOP(" ++ show k ++ "));" | (k, v) <- zip [1 :: Int ..] (vfs ++ vxs) ] ++
            [ "NODEPTR " ++ vr ++ " = TOP(" ++ show (arity - 1) ++ ");"
            , "POP(" ++ show arity ++ ");"
            , "(void)" ++ vk ++ "; (void)" ++ vr ++ ";" ]
  emitFun ("static NODEPTR " ++ fn ++ "(void) /* " ++ cComment name ++ " */") pre code
  modify $ \ s -> s{ gsEntries = (j, arity, bound, fn, name) : gsEntries s }
  return j

cComment :: String -> String
cComment = concatMap (\ c -> if c == '*' then "\\x2a" else [c])

-- A piece: a function for the rest of the code, after something has been
-- evaluated.  Its parameters are the variables of env that it uses, and the
-- extra ones, the result of the evaluation.  Returns the function name and
-- the parameters from env.
mkPiece :: Env -> [(CVar, String)] -> G Code -> G (String, [(CVar, String)])
mkPiece env extra gen = do
  saved <- gets gsUses
  modify $ \ s -> s{ gsUses = [] }
  code <- gen
  used <- gets gsUses
  let avail = nubBy (\ a b -> fst a == fst b) $ concatMap (cvarsOf . snd) (M.toList env)
      params = [ p | p@(c, _) <- avail, c `elem` used ]
  modify $ \ s -> s{ gsUses = foldr (\ c cs -> if c `elem` cs then cs else c : cs) saved (map fst params) }
  fn <- fresh "aot_p_"
  bound <- allocBound code
  modify $ \ s -> s{ gsBounds = (fn, bound) : gsBounds s }
  let decl (c, t) = t ++ " " ++ c
  emitFun ("static NODEPTR " ++ fn ++ "(" ++ intercalate ", " (map decl (params ++ extra)) ++ ")") [] code
  return (fn, params)

-- Save the parameters of a piece on the stack, push the marker for its
-- continuation, and return the node to evaluate.
saveAndEval :: [(CVar, String)] -> Int -> String -> Code
saveAndEval params c n =
  CSeq $ [ if t == "value_t" then CS 1 ("PUSH(mkInt(" ++ v ++ "));") else CS 0 ("PUSH(" ++ v ++ ");") | (v, t) <- params ] ++
         [ CS 0 ("PUSH(mk_AOT(V_AOTK, " ++ show c ++ "));"), CRet n ]

-- The continuation: restore the parameters and call the piece.
-- The result is popped by pre.
mkCont :: String -> [(CVar, String)] -> [String] -> [String] -> G Int
mkCont piece params pre extraArgs = do
  c <- gets gsNCont
  modify $ \ s -> s{ gsNCont = c + 1 }
  bs <- gets gsBounds
  let m = length params
      restore (i, (v, t)) | t == "value_t" = "value_t " ++ v ++ " = aot_getint(TOP(" ++ show (m - 1 - i) ++ "));"
                          | otherwise      = "NODEPTR " ++ v ++ " = TOP(" ++ show (m - 1 - i) ++ ");"
      fn = "aot_k_" ++ show c
      body = pre ++ map restore (zip [0 ..] params) ++ ["POP(" ++ show m ++ ");"]
      bound = fromMaybe (error "aot mkCont") (lookup piece bs)
  emitFun ("static NODEPTR " ++ fn ++ "(int k, int nn)") (body ++ ["(void)k; (void)nn;"])
          (CCall piece (map fst params ++ extraArgs))
  modify $ \ s -> s{ gsConts = (c, bound, fn) : gsConts s }
  return c

------------------------------------------------------------------------------

-- Code in tail position: compute the value of e, overwrite the redex with it,
-- and return the next node to evaluate.
cTail :: Env -> Exp -> G Code
cTail env ae
  | noFree env ae = do
      -- A constant, shared between calls.
      (c, n) <- cBuild env ae
      r <- redex env
      return $ CSeq [c, CRet $ "aot_ret_ind(" ++ r ++ ", " ++ n ++ ")"]
  | otherwise = cTail' env ae

-- As cTail, but compile e even if it has no local variables.  For the body
-- of an entry made for a constant, see closedify.
cTail' :: Env -> Exp -> G Code
cTail' env ae
  | Just (alts, scrut) <- getCase ae = cCase env ae alts scrut
  | Just (op, a, b) <- getBinOp ae =
      cIntOp env op a b $ \ env' res -> do
        r <- redex env'
        case res of
          Left i  -> return $ CRet $ "aot_ret_int(" ++ r ++ ", " ++ i ++ ")"
          Right n -> return $ CRet $ "aot_ret_ind(" ++ r ++ ", " ++ n ++ ")"
  | otherwise =
    case ae of
      App (Lam x b) e -> do
        (c, n) <- cBuild env e
        code <- cTail (M.insert x (VNode n) env) b
        return $ CSeq [c, code]
      Lit (LInt i) -> do
        r <- redex env
        return $ CRet $ "aot_ret_int(" ++ r ++ ", " ++ cInt i ++ ")"
      Var x | isLocal env x -> do
        mv <- lookupVar env x
        r <- redex env
        case mv of
          Just (VNode n) -> return $ CRet $ "aot_ret_ind(" ++ r ++ ", " ++ n ++ ")"
          Just (VBoth n _) -> do { use n; return $ CRet $ "aot_ret_ind(" ++ r ++ ", " ++ n ++ ")" }
          Just (VInt i) -> return $ CRet $ "aot_ret_int(" ++ r ++ ", " ++ i ++ ")"
          Nothing -> error "aot cTail"
      _ -> cTailApp env ae

-- Build e, in tail position.  An application is built in the redex.
cTailApp :: Env -> Exp -> G Code
cTailApp env (App f a) = do
  (cf, nf) <- cBuild env f
  (ca, na) <- cBuild env a
  r <- redex env
  return $ CSeq [cf, ca, CRet $ "aot_ret_ap(" ++ r ++ ", " ++ nf ++ ", " ++ na ++ ")"]
cTailApp env e = do
  (c, n) <- cBuild env e
  r <- redex env
  return $ CSeq [c, CRet $ "aot_ret_ind(" ++ r ++ ", " ++ n ++ ")"]

-- Build the graph for e, lazily.  Returns the code and the C variable.
cBuild :: Env -> Exp -> G (Code, CVar)
cBuild env ae
  | not (noFree env ae), Just (op, _, _) <- getBinOp ae, isJust (lookup op arithOps) = do
    -- Cheap eagerness: arithmetic on evaluated Ints cannot fail.
    mk <- knownInt env ae
    case mk of
      Just i -> do
        t <- fresh "n"
        return (CS 1 ("NODEPTR " ++ t ++ " = mkInt(" ++ i ++ ");"), t)
      _ -> cBuild' env ae
  | otherwise = cBuild' env ae

cBuild' :: Env -> Exp -> G (Code, CVar)
cBuild' env ae =
  case ae of
    Var x | isLocal env x -> do
      mv <- lookupVar env x
      case mv of
        Just (VNode n) -> return (CSeq [], n)
        Just (VBoth n _) -> do { use n; return (CSeq [], n) }
        Just (VInt i) -> do
          t <- fresh "n"
          return (CS 1 ("NODEPTR " ++ t ++ " = mkInt(" ++ i ++ ");"), t)
        Nothing -> error "aot cBuild"
    _ | noFree env ae -> do
      e <- closedify ae
      k <- constant env e
      t <- fresh "n"
      return (CS 0 ("NODEPTR " ++ t ++ " = " ++ k ++ ";"), t)
    _ | Just (op, a, b) <- getBinOp ae, op /= "icmp", isNothing (lookup op arithOps) -> do
      -- A comparison of known Ints is a constructor, so no allocation.
      ma <- knownInt env a
      mb <- knownInt env b
      case (ma, mb, lookup op cmpOps) of
        (Just x, Just y, Just cop) -> do
          t <- fresh "n"
          return (CS 0 ("NODEPTR " ++ t ++ " = AOT_BOOL(" ++ x ++ " " ++ cop ++ " " ++ y ++ ");"), t)
        _ -> cBuildApp env ae
    Lam _ _ -> do
      let (xs, b) = getLams ae
      cLift env xs b
    App (Lam _ _) _ -> cLift env [] ae
    _ | isJust (getCase ae) -> cLift env [] ae
    _ -> cBuildApp env ae

cBuildApp :: Env -> Exp -> G (Code, CVar)
cBuildApp env ae =
  case ae of
    App f a -> do
      (cf, nf) <- cBuild env f
      (ca, na) <- cBuild env a
      t <- fresh "n"
      return (CSeq [cf, ca, CS 1 ("NODEPTR " ++ t ++ " = new_ap(" ++ nf ++ ", " ++ na ++ ");")], t)
    _ -> do
      -- A literal, or a global, e.g., under a lambda that has been lifted.
      k <- constant env ae
      t <- fresh "n"
      return (CS 0 ("NODEPTR " ++ t ++ " = " ++ k ++ ";"), t)

-- Make an entry for \ xs -> b, and build its application to the array and
-- the free variables.
cLift :: Env -> [Ident] -> Exp -> G (Code, CVar)
cLift env xs b = do
  let fvs = nub [ x | x <- freeVars (lams xs b), isLocal env x ]
  -- The entry has an array of its own, which is a constant of this one.
  -- So a closure keeps only the constants it uses alive, as with bracket
  -- abstraction, where the code of a lambda is part of the code around it.
  saved <- gets gsConsts
  modify $ \ s -> s{ gsConsts = [] }
  d <- gets gsDef
  j <- genEntry (d ++ "/lifted") fvs xs b
  cs <- gets (reverse . gsConsts)
  modify $ \ s -> s{ gsConsts = saved }
  k <- constant env (apps (litPrim "aot$arr") cs)
  t0 <- fresh "n"
  let start = CS 1 ("NODEPTR " ++ t0 ++ " = new_ap(mk_AOT(V_AOT, " ++ show j ++ "), " ++ k ++ ");")
      app (cs, t) x = do
        (c, n) <- cBuild env (Var x)
        t' <- fresh "n"
        return (cs ++ [c, CS 1 ("NODEPTR " ++ t' ++ " = new_ap(" ++ t ++ ", " ++ n ++ ");")], t')
  (cs, t) <- foldM app ([start], t0) fvs
  return (CSeq cs, t)

-- Evaluate an Int expression, and go on with k, which gets the C expression
-- for it.
cIntE :: Env -> Exp -> (Env -> String -> G Code) -> G Code
cIntE env ae k = do
  mk <- knownInt env ae
  case mk of
    Just i -> k env i
    Nothing ->
      case ae of
        Var x | Just (VNode n) <- M.lookup x env -> do
          use n
          evalInt env (Just x) n k
        _ | Just (op, a, b) <- getBinOp ae, Just _ <- lookup op arithOps ->
          cIntOp env op a b $ \ env' res ->
            case res of
              Left i -> k env' i
              Right _ -> error "aot cIntE"
        _ -> do
          (c, n) <- cBuild env ae
          code <- evalInt env Nothing n k
          return $ CSeq [c, code]

-- Evaluate node n to an Int, and go on with k.
evalInt :: Env -> Maybe Ident -> CVar -> (Env -> String -> G Code) -> G Code
evalInt env mx n k = do
  t <- fresh "i"
  -- The value is in the environment, so that later pieces get it.
  let env' = case mx of
               Just x  -> M.insert x (VBoth n t) env
               Nothing -> M.insert (mkIdent ("aot$" ++ t)) (VInt t) env
  (piece, params) <- mkPiece env [(t, "value_t")] (k env' t)
  c <- mkCont piece params ["value_t " ++ t ++ " = aot_getint(TOP(0));", "POP(1);"] [t]
  use n
  return $ CSeq [ CS 0 ("value_t " ++ t ++ ";")
                , CIf ("aot_int(" ++ n ++ ", &" ++ t ++ ")")
                      (CCall piece (map fst params ++ [t]))
                      (saveAndEval params c n) ]

-- A binary Int operation.  The result is Left an Int, or Right a node (a
-- comparison).
cIntOp :: Env -> String -> Exp -> Exp -> (Env -> Either String String -> G Code) -> G Code
cIntOp env op a b k =
  cIntE env a $ \ env1 x ->
  cIntE env1 b $ \ env2 y -> do
    useExpr x
    useExpr y
    t <- fresh "v"
    -- The result is in the environment, so that later pieces get it.
    let envi = M.insert (mkIdent ("aot$" ++ t)) (VInt t) env2
        envn = M.insert (mkIdent ("aot$" ++ t)) (VNode t) env2
    case (lookup op arithOps, lookup op cmpOps) of
      (Just f, _) -> do
        code <- k envi (Left t)
        return $ CSeq [CS 0 ("value_t " ++ t ++ " = " ++ f ++ "(" ++ x ++ ", " ++ y ++ ");"), code]
      (_, Just cop) -> do
        code <- k envn (Right t)
        return $ CSeq [CS 0 ("NODEPTR " ++ t ++ " = AOT_BOOL(" ++ x ++ " " ++ cop ++ " " ++ y ++ ");"), code]
      _ -> do -- icmp
        code <- k envn (Right t)
        return $ CSeq [CS 0 ("NODEPTR " ++ t ++ " = AOT_ORD(AOT_CMP(" ++ x ++ ", " ++ y ++ "));"), code]

-- A case in tail position.
cCase :: Env -> Exp -> Alts -> Exp -> G Code
cCase env ae alts scrut
  -- if on an Int comparison
  | Dense [0, 0] [f, t] <- alts, Just (op, a, b) <- getBinOp scrut, Just cop <- lookup op cmpOps =
    cIntE env a $ \ env1 x ->
    cIntE env1 b $ \ env2 y -> do
      useExpr x
      useExpr y
      ct <- cTail env2 t
      cf <- cTail env2 f
      return $ CIf (x ++ " " ++ cop ++ " " ++ y) ct cf
  -- case on an Int compare
  | Dense [0, 0, 0] [lt, eq, gt] <- alts, Just ("icmp", a, b) <- getBinOp scrut =
    cIntE env a $ \ env1 x ->
    cIntE env1 b $ \ env2 y -> do
      useExpr x
      useExpr y
      clt <- cTail env2 lt
      ceq <- cTail env2 eq
      cgt <- cTail env2 gt
      return $ CIf (x ++ " < " ++ y) clt (CIf (x ++ " == " ++ y) ceq cgt)
  | otherwise = do
    -- The scrutinee, and whether it may already be evaluated.
    (cs, n, maybeEvald) <-
      case scrut of
        Var x | isLocal env x -> do { n <- getNode env x; return (CSeq [], n, True) }
        _ | noFree env scrut -> do { (c, n) <- cBuild env scrut; return (c, n, True) }
        _ -> do { (c, n) <- cBuild env scrut; return (c, n, False) }
    ck <- fresh "ck"
    cf <- fresh "cf"
    let arms = case alts of
                 Dense as fs -> [ (k, a, f) | (k, a, f) <- zip3 [0 ..] as fs ]
                 Sparse _ afs -> afs
        dflt = case alts of
                 Dense _ _ -> return $ CS 0 "ERR(\"aot: bad constructor\");\nreturn NIL_NODEPTR;"
                 Sparse d _ -> cTail env d
        arm (k, a, f) = do
          vs <- mapM (\ i -> do { v <- fresh "x"; return (v, i) }) [0 .. a - 1]
          code <- cArm env f (map fst vs)
          return (k, CSeq ([ CS 0 ("NODEPTR " ++ v ++ " = " ++ cf ++ "[" ++ show i ++ "];") | (v, i) <- vs ] ++ [code]))
        maxf = maximum (1 : [ a | (_, a, _) <- arms ])
    if maxf > 16 then cTailApp env ae else do
      (piece, params) <- mkPiece env [(ck, "int"), (cf, "NODEPTR *")] $ do
        codes <- mapM arm arms
        d <- dflt
        mapM_ use [ck, cf]
        return $ CSwitch ck codes d
      c <- mkCont piece params
             [ "NODEPTR " ++ cf ++ "[AOT_MAXF];"
             , "for (int i = 0; i < nn && i < AOT_MAXF; i++) " ++ cf ++ "[i] = ARG(TOP(i));"
             , "POP(nn + 1);" ]
             ["k", cf]
      use n
      let slow = saveAndEval params c n
      return $ CSeq $ [cs] ++
        if maybeEvald then
          [ CS 0 ("NODEPTR " ++ cf ++ "[AOT_MAXF];\nint " ++ ck ++ " = aot_con(" ++ n ++ ", " ++ cf ++ ", AOT_MAXF);")
          , CIf (ck ++ " >= 0") (CCall piece (map fst params ++ [ck, cf])) slow ]
        else
          [slow]

-- An arm: bind the fields, and go on in tail position.
cArm :: Env -> Exp -> [CVar] -> G Code
cArm env (Lam x b) (v : vs) = cArm (M.insert x (VNode v) env) b vs
cArm env e [] = cTail env e
cArm env e vs = do
  xs <- mapM (const (fresh "aot$y")) vs
  let ids = map mkIdent xs
  cTail (foldr (\ (x, v) -> M.insert x (VNode v)) env (zip ids vs)) (apps e (map Var ids))

------------------------------------------------------------------------------

cFile :: GS -> String
cFile s =
  unlines $
    [ "/* Generated by mhs --aot, see aot_entries in eval.c */" ] ++
    reverse (gsProtos s) ++
    reverse (gsFuns s) ++
    [ "static const struct aot_entry aot_entries[] = {" ] ++
    [ "  { " ++ show a ++ ", " ++ show b ++ ", " ++ f ++ ", " ++ show n ++ " }," | (_, a, b, f, n) <- sortOn (\ (j, _, _, _, _) -> j) (gsEntries s) ] ++
    [ "};"
    , "static const struct aot_cont aot_conts[] = {" ] ++
    [ "  { " ++ show b ++ ", " ++ f ++ " }," | (_, b, f) <- sortOn (\ (c, _, _) -> c) (gsConts s) ] ++
    [ "  { 0, 0 }"
    , "};"
    , "#define AOT_NENTRIES " ++ show (gsNEntry s) ]
