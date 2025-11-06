module Data.Data_Class(module Data.Data_Class) where
import qualified Prelude()
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function(($), (.), const, id)
import Data.Functor.Const(Const(..))
import Data.Functor.Identity(Identity(..))
import Data.Int
import Data.Integer
import Data.List((++))
import Data.Maybe
import Data.Num
import Data.Ratio
import {-# SOURCE #-} Data.Typeable
import Text.Show

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
      k c x = Const $ getConst c `o` f x
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


data DataType = DataType {
    tycon   :: String
  , datarep :: DataRep
  }
--  deriving Show

data Constr = Constr {
    conrep    :: ConstrRep
  , constring :: String
  , confields :: [String] -- for AlgRep only
  , confixity :: Fixity   -- for AlgRep only
  , datatype  :: DataType
  }

data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep
--            deriving (Eq, Show)

data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char
--               deriving (Eq, Show)

type ConIndex = Int

data Fixity = Prefix
            | Infix
--            deriving (Eq, Show)

mkDataType :: String -> [Constr] -> DataType
mkDataType str cs =
  DataType {
    tycon   = str
  , datarep = AlgRep cs
  }

mkConstrTag :: DataType -> String -> Int -> [String] -> Fixity -> Constr
mkConstrTag dt str idx fields fix =
  Constr {
    conrep    = AlgConstr idx
  , constring = str
  , confields = fields
  , confixity = fix
  , datatype  = dt
  }

constrIndex :: Constr -> ConIndex
constrIndex con =
  case conrep con of
    AlgConstr idx -> idx
    _ -> error $ "Data.Data.constrIndex is not supported for "
                 ++ tycon (datatype con)
                 ++ ", as it is not an algebraic data type."
