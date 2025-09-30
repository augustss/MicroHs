module Control.WrappedMonad(WrappedMonad(..), WrappedArrow(..)) where
import qualified Prelude(); import MiniPrelude
import Control.Applicative
import Control.Arrow
import Mhs.Builtin

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }
  deriving (Monad)

instance Monad m => Functor (WrappedMonad m) where
  fmap f (WrapMonad v) = WrapMonad (fmap f v)

instance Monad m => Applicative (WrappedMonad m) where
  pure = WrapMonad . pure
  WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
  liftA2 f (WrapMonad x) (WrapMonad y) = WrapMonad (liftM2 f x y)

instance MonadPlus m => Alternative (WrappedMonad m) where
  empty = WrapMonad mzero
  WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }

instance Arrow a => Functor (WrappedArrow a b) where
  fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

instance Arrow a => Applicative (WrappedArrow a b) where
  pure x = WrapArrow (arr (const x))
  liftA2 f (WrapArrow u) (WrapArrow v) =
    WrapArrow (u &&& v >>> arr (uncurry f))

instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) where
  empty = WrapArrow zeroArrow
  WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)
