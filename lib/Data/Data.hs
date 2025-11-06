{-
module Data.Data (
  Data(..),
  DataType,
  mkDataType,
  mkIntType,
  mkFloatType,
  mkCharType,
  mkNoRepType,
  dataTypeName,
  DataRep(..),
  dataTypeRep,
  repConstr,
  isAlgType,
  dataTypeConstrs,
  indexConstr,
  maxConstrIndex,
  isNorepType,
  Constr,
  ConIndex,
  Fixity(..),
  mkConstr,
  mkConstrTag,
  mkIntegralConstr,
  mkRealConstr,
  mkCharConstr,
  constrType,
  ConstrRep(..),
  constrRep,
  constrFields,
  constrFixity,
  constrIndex,
  showConstr,
  readConstr,
  tyconUQname,
  tyconModule,
  fromConstr,
  fromConstrB,
  fromConstrM,
  ) where
import Control.Monad
import Data.Functor.Const(Const(..))
import Data.Functor.Identity(Identity(..))
import Data.Int(Int8, Int16, Int32, Int64)
import Data.Ix(Ix)
import Data.List(findIndex)
import Data.Maybe(fromJust)
import Data.Ratio(Ratio)
import Data.Typeable
import Data.Word(Word, Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr(ForeignPtr)
import Foreign.Ptr(Ptr)
import Mhs.Array(Array)
import Numeric.Natural(Natural)

class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a
          -> c a

  gfoldl _ z = z

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr   :: a -> Constr

  dataTypeOf  :: a -> DataType

  dataCast1 :: Typeable t
            => (forall d. Data d => c (t d))
            -> Maybe (c a)
  dataCast1 _ = Nothing

  dataCast2 :: Typeable t
            => (forall d e. (Data d, Data e) => c (t d e))
            -> Maybe (c a)
  dataCast2 _ = Nothing

  gmapT :: (forall b. Data b => b -> b) -> a -> a

  gmapT f x0 = runIdentity (gfoldl k Identity x0)
    where
      k :: Data d => Identity (d->b) -> d -> Identity b
      k (Identity c) x = Identity (c (f x))

  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQl o r f = getConst . gfoldl k z
    where
      k :: Data d => Const r (d->b) -> d -> Const r b
      k c x = Const $ (getConst c) `o` f x
      z :: g -> Const r g
      z _   = Const r

  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k :: Data d => Qr r (d->b) -> d -> Qr r b
      k (Qr c) x = Qr (\r -> c (f x `o` r))

  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f

  gmapQi :: forall u. Int -> (forall d. Data d => d -> u) -> a -> u
  gmapQi i f x = case gfoldl k z x of { Qi _ q -> fromJust q }
    where
      k :: Data d => Qi u (d -> b) -> d -> Qi u b
      k (Qi i' q) a = Qi (i'+1) (if i==i' then Just (f a) else q)
      z :: g -> Qi q g
      z _           = Qi 0 Nothing

  gmapM :: forall m. Monad m => (forall d. Data d => d -> m d) -> a -> m a
  gmapM f = gfoldl k return
    where
      k :: Data d => m (d -> b) -> d -> m b
      k c x = do c' <- c
                 x' <- f x
                 return (c' x')

  gmapMp :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a
  gmapMp f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h, b) ->
                 (f y >>= \y' -> return (h y', True))
                 `mplus` return (h y, b)
             )

  gmapMo :: forall m. MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a
  gmapMo f x = unMp (gfoldl k z x) >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> Mp m g
      z g = Mp (return (g,False))
      k :: Data d => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y
        = Mp ( c >>= \(h,b) -> if b
                        then return (h y, b)
                        else (f y >>= \y' -> return (h y',True))
                             `mplus` return (h y, b)
             )


data Qi q a = Qi Int (Maybe q)

newtype Qr r a = Qr { unQr  :: r -> r }

newtype Mp m x = Mp { unMp :: m (x, Bool) }

fromConstr :: Data a => Constr -> a
fromConstr = fromConstrB (error "Data.Data.fromConstr")

fromConstrB :: Data a
            => (forall d. Data d => d)
            -> Constr
            -> a
fromConstrB f = runIdentity . gunfold k z
 where
  k :: forall b r. Data b => Identity (b -> r) -> Identity r
  k c = Identity (runIdentity c f)

  z :: forall r. r -> Identity r
  z = Identity

fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d)
            -> Constr
            -> m a
fromConstrM f = gunfold k z
 where
  k :: forall b r. Data b => m (b -> r) -> m r
  k c = do { c' <- c; b <- f; return (c' b) }

  z :: forall r. r -> m r
  z = return

data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

              deriving Show

data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
                        , datatype  :: DataType
                        }

instance Show Constr where
 show = constring

instance Eq Constr where
  c == c' = constrRep c == constrRep c'

data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep
            deriving (Eq, Show)

data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char
               deriving (Eq, Show)

type ConIndex = Int

data Fixity = Prefix
            | Infix
            deriving (Eq, Show)

dataTypeName :: DataType -> String
dataTypeName = tycon

dataTypeRep :: DataType -> DataRep
dataTypeRep = datarep

constrType :: Constr -> DataType
constrType = datatype

constrRep :: Constr -> ConstrRep
constrRep = conrep

repConstr :: DataType -> ConstrRep -> Constr
repConstr dt cr =
      case (dataTypeRep dt, cr) of
        (AlgRep cs, AlgConstr i)      -> cs !! (i-1)
        (IntRep,    IntConstr i)      -> mkIntegralConstr dt i
        (FloatRep,  FloatConstr f)    -> mkRealConstr dt f
        (CharRep,   CharConstr c)     -> mkCharConstr dt c
        _ -> error "Data.Data.repConstr: The given ConstrRep does not fit to the given DataType."

mkDataType :: String -> [Constr] -> DataType
mkDataType str cs = DataType
                        { tycon   = str
                        , datarep = AlgRep cs
                        }

mkConstrTag :: DataType -> String -> Int -> [String] -> Fixity -> Constr
mkConstrTag dt str idx fields fix =
        Constr
                { conrep    = AlgConstr idx
                , constring = str
                , confields = fields
                , confixity = fix
                , datatype  = dt
                }

mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dt str fields fix = mkConstrTag dt str idx fields fix
  where
    idx = case findIndex (\c -> showConstr c == str) (dataTypeConstrs dt) of
            Just i  -> i+1 -- ConTag starts at 1
            Nothing -> error $ "Data.Data.mkConstr: couldn't find constructor " ++ str


dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dt = case datarep dt of
                        (AlgRep cons) -> cons
                        _ -> error $ "Data.Data.dataTypeConstrs is not supported for "
                                    ++ dataTypeName dt ++
                                    ", as it is not an algebraic data type."

constrFields :: Constr -> [String]
constrFields = confields

constrFixity :: Constr -> Fixity
constrFixity = confixity

showConstr :: Constr -> String
showConstr = constring

readConstr :: DataType -> String -> Maybe Constr
readConstr dt str =
      case dataTypeRep dt of
        AlgRep cons -> idx cons
        IntRep      -> mkReadCon (\i -> (mkPrimCon dt str (IntConstr i)))
        FloatRep    -> mkReadCon ffloat
        CharRep     -> mkReadCon (\c -> (mkPrimCon dt str (CharConstr c)))
        NoRep       -> Nothing
  where

    -- Read a value and build a constructor
    mkReadCon :: Read t => (t -> Constr) -> Maybe Constr
    mkReadCon f = case (reads str) of
                    [(t,"")] -> Just (f t)
                    _ -> Nothing

    -- Traverse list of algebraic datatype constructors
    idx :: [Constr] -> Maybe Constr
    idx cons = case filter ((==) str . showConstr) cons of
                [] -> Nothing
                hd : _ -> Just hd

    ffloat :: Double -> Constr
    ffloat =  mkPrimCon dt str . FloatConstr . toRational

isAlgType :: DataType -> Bool
isAlgType dt = case datarep dt of
                 (AlgRep _) -> True
                 _ -> False

indexConstr :: DataType -> ConIndex -> Constr
indexConstr dt idx = case datarep dt of
                        (AlgRep cs) -> cs !! (idx-1)
                        _           -> error $ "Data.Data.indexConstr is not supported for "
                                               ++ dataTypeName dt ++
                                               ", as it is not an algebraic data type."

constrIndex :: Constr -> ConIndex
constrIndex con = case constrRep con of
                    (AlgConstr idx) -> idx
                    _ -> error $ "Data.Data.constrIndex is not supported for "
                                 ++ dataTypeName (constrType con) ++
                                 ", as it is not an algebraic data type."

maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dt = case dataTypeRep dt of
                        AlgRep cs -> length cs
                        _            -> error $ "Data.Data.maxConstrIndex is not supported for "
                                                 ++ dataTypeName dt ++
                                                 ", as it is not an algebraic data type."


mkIntType :: String -> DataType
mkIntType = mkPrimType IntRep

mkFloatType :: String -> DataType
mkFloatType = mkPrimType FloatRep

mkCharType :: String -> DataType
mkCharType = mkPrimType CharRep

mkPrimType :: DataRep -> String -> DataType
mkPrimType dr str = DataType
                        { tycon   = str
                        , datarep = dr
                        }

mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dt str cr = Constr
                        { datatype  = dt
                        , conrep    = cr
                        , constring = str
                        , confields = error "Data.Data.confields"
                        , confixity = error "Data.Data.confixity"
                        }

mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkIntegralConstr dt i = case datarep dt of
                  IntRep -> mkPrimCon dt (show i) (IntConstr (toInteger  i))
                  _ -> error $ "Data.Data.mkIntegralConstr is not supported for "
                               ++ dataTypeName dt ++
                               ", as it is not an Integral data type."

mkRealConstr :: (Real a, Show a) => DataType -> a -> Constr
mkRealConstr dt f = case datarep dt of
                    FloatRep -> mkPrimCon dt (show f) (FloatConstr (toRational f))
                    _ -> error $ "Data.Data.mkRealConstr is not supported for "
                                 ++ dataTypeName dt ++
                                 ", as it is not a Real data type."

mkCharConstr :: DataType -> Char -> Constr
mkCharConstr dt c = case datarep dt of
                   CharRep -> mkPrimCon dt (show c) (CharConstr c)
                   _ -> error $ "Data.Data.mkCharConstr is not supported for "
                                ++ dataTypeName dt ++
                                ", as it is not an Char data type."

mkNoRepType :: String -> DataType
mkNoRepType str = DataType
                        { tycon   = str
                        , datarep = NoRep
                        }

isNorepType :: DataType -> Bool
isNorepType dt = case datarep dt of
                   NoRep -> True
                   _ -> False

tyconUQname :: String -> String
tyconUQname x = case dropWhile (not . (==) '.') x of
                  [] -> x
                  _ : tl -> tyconUQname tl

tyconModule :: String -> String
tyconModule x = case break ((==) '.') x of
                  (_, "") -> ""
                  (a, _ : tl) -> a ++ tyconModule' tl
  where
    tyconModule' y = let y' = tyconModule y
                      in if y' == "" then "" else ('.':y')

-- NOT YET deriving instance Data Bool

------------------------------------------------------------------------------

charType :: DataType
charType = mkCharType "Prelude.Char"

instance Data Char where
  toConstr x = mkCharConstr charType x
  gunfold _ z c = case constrRep c of
                    (CharConstr x) -> z x
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Char."
  dataTypeOf _ = charType


------------------------------------------------------------------------------

floatType :: DataType
floatType = mkFloatType "Prelude.Float"

instance Data Float where
  toConstr = mkRealConstr floatType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Float."
  dataTypeOf _ = floatType


------------------------------------------------------------------------------

doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"

instance Data Double where
  toConstr = mkRealConstr doubleType
  gunfold _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Double."
  dataTypeOf _ = doubleType


------------------------------------------------------------------------------

intType :: DataType
intType = mkIntType "Prelude.Int"

instance Data Int where
  toConstr x = mkIntegralConstr intType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int."
  dataTypeOf _ = intType


------------------------------------------------------------------------------

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

instance Data Integer where
  toConstr = mkIntegralConstr integerType
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Integer."
  dataTypeOf _ = integerType


------------------------------------------------------------------------------

naturalType :: DataType
naturalType = mkIntType "Numeric.Natural.Natural"

instance Data Natural where
  toConstr x = mkIntegralConstr naturalType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Natural"
  dataTypeOf _ = naturalType


------------------------------------------------------------------------------

int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"

instance Data Int8 where
  toConstr x = mkIntegralConstr int8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int8."
  dataTypeOf _ = int8Type


------------------------------------------------------------------------------

int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"

instance Data Int16 where
  toConstr x = mkIntegralConstr int16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int16."
  dataTypeOf _ = int16Type


------------------------------------------------------------------------------

int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"

instance Data Int32 where
  toConstr x = mkIntegralConstr int32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int32."
  dataTypeOf _ = int32Type


------------------------------------------------------------------------------

int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"

instance Data Int64 where
  toConstr x = mkIntegralConstr int64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int64."
  dataTypeOf _ = int64Type


------------------------------------------------------------------------------

wordType :: DataType
wordType = mkIntType "Data.Word.Word"

instance Data Word where
  toConstr x = mkIntegralConstr wordType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word"
  dataTypeOf _ = wordType


------------------------------------------------------------------------------

word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"

instance Data Word8 where
  toConstr x = mkIntegralConstr word8Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word8."
  dataTypeOf _ = word8Type


------------------------------------------------------------------------------

word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"

instance Data Word16 where
  toConstr x = mkIntegralConstr word16Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word16."
  dataTypeOf _ = word16Type


------------------------------------------------------------------------------

word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"

instance Data Word32 where
  toConstr x = mkIntegralConstr word32Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word32."
  dataTypeOf _ = word32Type


------------------------------------------------------------------------------

word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"

instance Data Word64 where
  toConstr x = mkIntegralConstr word64Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word64."
  dataTypeOf _ = word64Type


------------------------------------------------------------------------------

ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix

ratioDataType :: DataType
ratioDataType = mkDataType "Data.Ratio" [ratioConstr]

-- NB: This Data instance intentionally uses the (%) smart constructor instead
-- of the internal (:%) constructor to preserve the invariant that a Ratio
-- value is reduced to normal form. See #10011.

-- | @since base-4.0.0.0
instance (Data a, Integral a) => Data (Ratio a) where
  gfoldl k z (a :% b) = z (%) `k` a `k` b
  toConstr _ = ratioConstr
  gunfold k z c | constrIndex c == 1 = k (k (z (%)))
  gunfold _ _ _ = error "Data.Data.gunfold(Ratio)"
  dataTypeOf _  = ratioDataType


------------------------------------------------------------------------------

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> error "Data.Data.gunfold(List)"
  dataTypeOf _ = listDataType
  dataCast1 f  = gcast1 f

  gmapT  _   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  _   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  _   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')


------------------------------------------------------------------------------
{- NOT YET
-- | @since base-4.9.0.0
deriving instance Data a => Data (NonEmpty a)

-- | @since base-4.0.0.0
deriving instance Data a => Data (Maybe a)

-- | @since base-4.0.0.0
deriving instance Data Ordering

-- | @since base-4.0.0.0
deriving instance (Data a, Data b) => Data (Either a b)

-- | @since base-4.8.0.0
deriving instance Data Void

-- | @since base-4.0.0.0
deriving instance Data ()

-- | @since base-4.15
deriving instance Data a => Data (Solo a)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b) => Data (a,b)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b, Data c) => Data (a,b,c)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f)

-- | @since base-4.0.0.0
deriving instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g)
-}
------------------------------------------------------------------------------

-- | @since base-4.8.0.0
instance Data a => Data (Ptr a) where
  toConstr _   = error "Data.Data.toConstr(Ptr)"
  gunfold _ _  = error "Data.Data.gunfold(Ptr)"
  dataTypeOf _ = mkNoRepType "Primitives.Ptr"
  dataCast1 x  = gcast1 x

-- NOT YET deriving instance Data a => Data (ConstPtr a)

------------------------------------------------------------------------------

instance Data a => Data (ForeignPtr a) where
  toConstr _   = error "Data.Data.toConstr(ForeignPtr)"
  gunfold _ _  = error "Data.Data.gunfold(ForeignPtr)"
  dataTypeOf _ = mkNoRepType "Primitives.ForeignPtr"
  dataCast1 x  = gcast1 x

-- NOT YET deriving instance Data IntPtr

-- NOT YET deriving instance Data WordPtr

------------------------------------------------------------------------------
instance (Data a, Data b, Ix a) => Data (Array a b)
 where
  gfoldl f z a = z (listArray (bounds a)) `f` (elems a)
  toConstr _   = error "Data.Data.toConstr(Array)"
  gunfold _ _  = error "Data.Data.gunfold(Array)"
  dataTypeOf _ = mkNoRepType "Data.Array.Array"
  dataCast2 x  = gcast2 x

{- NOT YET
----------------------------------------------------------------------------
-- Data instance for Proxy

-- | @since base-4.7.0.0
deriving instance (Data t) => Data (Proxy t)

-- | @since base-4.7.0.0
deriving instance (a ~ b, Data a) => Data (a :~: b)

-- | @since base-4.10.0.0
deriving instance (Typeable i, Typeable j, Typeable a, Typeable b,
                    (a :: i) ~~ (b :: j))
    => Data (a :~~: b)

-- | @since base-4.7.0.0
deriving instance (Coercible a b, Data a, Data b) => Data (Coercion a b)

-- | @since base-4.9.0.0
deriving instance Data a => Data (Identity a)

-- | @since base-4.10.0.0
deriving instance (Typeable k, Data a, Typeable (b :: k)) => Data (Const a b)

-- | @since base-4.7.0.0
deriving instance Data Version

----------------------------------------------------------------------------
-- Data instances for GHC.Internal.Data.Monoid wrappers

-- | @since base-4.8.0.0
deriving instance Data a => Data (Dual a)

-- | @since base-4.8.0.0
deriving instance Data All

-- | @since base-4.8.0.0
deriving instance Data Any

-- | @since base-4.8.0.0
deriving instance Data a => Data (Sum a)

-- | @since base-4.8.0.0
deriving instance Data a => Data (Product a)

-- | @since base-4.8.0.0
deriving instance Data a => Data (First a)

-- | @since base-4.8.0.0
deriving instance Data a => Data (Last a)

-- | @since base-4.8.0.0
deriving instance (Data (f a), Data a, Typeable f) => Data (Alt f a)

-- | @since base-4.12.0.0
deriving instance (Data (f a), Data a, Typeable f) => Data (Ap f a)

----------------------------------------------------------------------------
-- Data instances for GHC.Generics representations

-- | @since base-4.9.0.0
deriving instance Data p => Data (U1 p)

-- | @since base-4.9.0.0
deriving instance Data p => Data (Par1 p)

-- | @since base-4.9.0.0
deriving instance (Data (f p), Typeable f, Data p) => Data (Rec1 f p)

-- | @since base-4.9.0.0
deriving instance (Typeable i, Data p, Data c) => Data (K1 i c p)

-- | @since base-4.9.0.0
deriving instance (Data p, Data (f p), Typeable c, Typeable i, Typeable f)
    => Data (M1 i c f p)

-- | @since base-4.9.0.0
deriving instance (Typeable f, Typeable g, Data p, Data (f p), Data (g p))
    => Data ((f :+: g) p)

-- | @since base-4.9.0.0
deriving instance (Typeable (f :: Type -> Type), Typeable (g :: Type -> Type),
          Data p, Data (f (g p)))
    => Data ((f :.: g) p)

-- | @since base-4.9.0.0
deriving instance Data p => Data (V1 p)

-- | @since base-4.9.0.0
deriving instance (Typeable f, Typeable g, Data p, Data (f p), Data (g p))
    => Data ((f :*: g) p)

-- | @since base-4.9.0.0
deriving instance Data Generics.Fixity

-- | @since base-4.9.0.0
deriving instance Data Associativity

-- | @since base-4.9.0.0
deriving instance Data SourceUnpackedness

-- | @since base-4.9.0.0
deriving instance Data SourceStrictness

-- | @since base-4.9.0.0
deriving instance Data DecidedStrictness

----------------------------------------------------------------------------
-- Data instances for GHC.Internal.Data.Ord

-- | @since base-4.12.0.0
deriving instance Data a => Data (Down a)
-}
-}

module Data.Data(Data(..), module Data.Typeable) where
import Data.Typeable

-- Fake Data class until the type checker bug has been fixed
class (Typeable a) => Data a
