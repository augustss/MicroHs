-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module AllOfLib(main) where
-- Only used to save the compilation cache.
import Control.Applicative
import Control.Category
import Control.DeepSeq
import Control.Error
import Control.Exception
import Control.Exception.Internal
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.ST_Type
import Data.Bits
import Data.Bool
import Data.Bool_Type
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Bounded
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy.Char8
import Data.Char
import Data.Char_Type
import Data.Coerce
import Data.Complex
import Data.Constraint
import Data.Dynamic
import Data.Either
import Data.Enum
import Data.Eq
import Data.Fixed
import Data.FloatW
import Data.Floating
import Data.Foldable
import Data.Foldable1
import Data.Fractional
import Data.Function
import Data.Functor
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Hashable
import Data.IOArray
import Data.IORef
import Data.Int
import Data.Int.IntN
import Data.Int.Instances
import Data.Integer
import Data.Integer_Type
import Data.Integral
import Data.Ix
import Data.Kind
import Data.List
import Data.List_Type
import Data.Maybe
import Data.Maybe_Type
import Data.Monoid
import Data.Monoid.Internal
import Data.Num
import Data.Ord
import Data.Orphans
import Data.Ordering_Type
import Data.Proxy
import Data.Ratio
import Data.Ratio_Type
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Records
import Data.Semigroup
import Data.STRef
import Data.String
import Data.Text
import Data.Text.IO
import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Traversable
import Data.Tuple
import Data.Tuple.Instances
import Data.Typeable
import Data.TypeLits
import Data.Version
import Data.Void
import Data.Word
import Data.ZipList
import Debug.Trace
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import GHC.Types
import Numeric
import Numeric.FormatFloat
import Numeric.Natural
import Prelude
import Primitives
import System.Compress
import System.Console.GetOpt
--import System.Console.SimpleReadline
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO_Handle
import System.IO.Error
import System.IO.MD5
import System.IO.PrintOrRun
import System.IO.Serialize
import System.IO.StringHandle
import System.IO.TimeMilli
import System.Info
import System.Process
import Text.Printf
import Text.Read
import Text.Show
import Unsafe.Coerce

main :: IO ()
main = return ()
