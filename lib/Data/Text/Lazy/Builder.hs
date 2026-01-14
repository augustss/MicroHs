module Data.Text.Lazy.Builder
  ( Builder
  , LazyTextBuilder
  , toLazyText
  , toLazyTextWith
  , singleton
  , fromText
  , fromLazyText
  , fromString
  , flush
  ) where
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.String

newtype Builder = Builder (L.Text -> L.Text)

type LazyTextBuilder = Builder

instance Eq Builder where
  a == b = toLazyText a == toLazyText b

instance Ord Builder where
  compare a b = compare (toLazyText a) (toLazyText b)

instance Semigroup Builder where
  Builder f <> Builder g = Builder (f . g)

instance Monoid Builder where
  mempty = Builder id

instance IsString Builder where
  fromString = fromLazyText . L.pack

instance Show Builder where
  show = show . toLazyText

toLazyText :: Builder -> L.Text
toLazyText (Builder f) = f L.empty

toLazyTextWith :: Int -> Builder -> L.Text
toLazyTextWith _ = toLazyText

singleton :: Char -> Builder
singleton = fromLazyText . L.singleton

fromText :: S.Text -> Builder
fromText = fromLazyText . L.toLazy

fromLazyText :: L.Text -> Builder
fromLazyText t = Builder (t `L.append`)

flush :: Builder
flush = Builder id
