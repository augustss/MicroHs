module Data.Bits(
  Bits(..),
  FiniteBits(..),
  bitDefault,
  testBitDefault,
  popCountDefault,
  toIntegralSized,
  oneBits,
  (.^.),
  (.>>.), (.<<.), (!>>.), (!<<.),
  And(..), Ior(..), Xor(..), Iff(..),
  ) where
import qualified Prelude()              -- do not import Prelude
import MiniPrelude
import Data.Bits.Base
import Data.Bounded
import Data.List as List
import Mhs.Builtin

oneBits :: (FiniteBits a) => a
oneBits = complement zeroBits

infixl 6 .^.
(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 8 .>>.
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .<<.
(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 !>>.
(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR

infixl 8 !<<.
(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL

--------

newtype And a = And { getAnd :: a }
  deriving newtype (
                    Bounded, -- ^ @since base-4.16
                    Enum, -- ^ @since base-4.16
                    Bits, -- ^ @since base-4.16
                    FiniteBits, -- ^ @since base-4.16
                    Eq -- ^ @since base-4.16
                    )
  deriving stock (
                  Show, -- ^ @since base-4.16
                  Read -- ^ @since base-4.16
                 )

-- | @since base-4.16
instance (Bits a) => Semigroup (And a) where
  And x <> And y = And (x .&. y)

-- | This constraint is arguably too strong. However,
-- as some types (such as 'Natural') have undefined 'complement', this is the
-- only safe choice.
--
-- @since base-4.16
instance (FiniteBits a) => Monoid (And a) where
  mempty = And oneBits
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | Monoid under bitwise inclusive OR.
--
-- >>> getIor (Ior 0xab <> Ior 0x12) :: Word8
-- 187
--
-- @since base-4.16
newtype Ior a = Ior { getIor :: a }
  deriving newtype (
                    Bounded, -- ^ @since base-4.16
                    Enum, -- ^ @since base-4.16
                    Bits, -- ^ @since base-4.16
                    FiniteBits, -- ^ @since base-4.16
                    Eq -- ^ @since base-4.16
                    )
  deriving stock (
                  Show, -- ^ @since base-4.16
                  Read -- ^ @since base-4.16
                 )

-- | @since base-4.16
instance (Bits a) => Semigroup (Ior a) where
  Ior x <> Ior y = Ior (x .|. y)

-- | @since base-4.16
instance (Bits a) => Monoid (Ior a) where
  mempty = Ior zeroBits
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | Monoid under bitwise XOR.
--
-- >>> getXor (Xor 0xab <> Xor 0x12) :: Word8
-- 185
--
-- @since base-4.16
newtype Xor a = Xor { getXor :: a }
  deriving newtype (
                    Bounded, -- ^ @since base-4.16
                    Enum, -- ^ @since base-4.16
                    Bits, -- ^ @since base-4.16
                    FiniteBits, -- ^ @since base-4.16
                    Eq -- ^ @since base-4.16
                    )
  deriving stock (
                  Show, -- ^ @since base-4.16
                  Read -- ^ @since base-4.16
                 )

-- | @since base-4.16
instance (Bits a) => Semigroup (Xor a) where
  Xor x <> Xor y = Xor (x `xor` y)

-- | @since base-4.16
instance (Bits a) => Monoid (Xor a) where
  mempty = Xor zeroBits
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | Monoid under bitwise \'equality\'; defined as @1@ if the corresponding
-- bits match, and @0@ otherwise.
--
-- >>> getIff (Iff 0xab <> Iff 0x12) :: Word8
-- 70
--
-- @since base-4.16
newtype Iff a = Iff { getIff :: a }
  deriving newtype (
                    Bounded, -- ^ @since base-4.16
                    Enum, -- ^ @since base-4.16
                    Bits, -- ^ @since base-4.16
                    FiniteBits, -- ^ @since base-4.16
                    Eq -- ^ @since base-4.16
                    )
  deriving stock (
                  Show, -- ^ @since base-4.16
                  Read -- ^ @since base-4.16
                 )

-- | This constraint is arguably
-- too strong. However, as some types (such as 'Natural') have undefined
-- 'complement', this is the only safe choice.
--
-- @since base-4.16
instance (FiniteBits a) => Semigroup (Iff a) where
  Iff x <> Iff y = Iff . complement $ (x `xor` y)

-- | This constraint is arguably
-- too strong. However, as some types (such as 'Natural') have undefined
-- 'complement', this is the only safe choice.
--
-- @since base-4.16
instance (FiniteBits a) => Monoid (Iff a) where
  mempty = Iff oneBits
  -- By default, we would get a lazy right fold. This forces the use of a strict
  -- left fold instead.
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}
