-- MicroHs does not distinguish lazy and strict bytestrings
module Data.ByteString.Lazy(module Data.ByteString.Lazy) where
import Primitives
import qualified Data.ByteString as B
import Data.ByteString.Internal(c2w)
import Data.Coerce
import qualified Data.List as List
import Data.List.NonEmpty
import Data.Int.Int64(Int64)
import Data.Word.Word8(Word8)
import Foreign.C.String(CString, CStringLen)
import System.IO.Base(Handle)

-- XXX: should be a lazy list of strict chunks
newtype ByteString = BS B.ByteString
  deriving (Eq, Ord)

instance IsString ByteString where
  fromString = pack . List.map c2w

empty :: ByteString
empty = coerce B.empty

singleton :: Word8 -> ByteString
singleton w = pack [w]

pack :: [Word8] -> ByteString
pack = coerce B.pack

unpack :: ByteString -> [Word8]
unpack = coerce B.unpack

fromStrict :: B.ByteString -> ByteString
fromStrict = BS

toStrict :: ByteString -> B.ByteString
toStrict (BS bs) = bs

fromChunks :: [B.ByteString] -> ByteString
fromChunks = coerce B.concat

toChunks :: ByteString -> [B.ByteString]
toChunks (BS bs) = [bs]

fromFilePath :: FilePath -> IO ByteString
fromFilePath = coerce B.fromFilePath

toFilePath :: ByteString -> IO FilePath
toFilePath = coerce B.toFilePath

cons :: Word8 -> ByteString -> ByteString
cons = coerce B.cons

snoc :: ByteString -> Word8 -> ByteString
snoc = coerce B.snoc

append :: ByteString -> ByteString -> ByteString
append = coerce B.append

head :: ByteString -> Word8
head = coerce B.head

tail :: ByteString -> ByteString
tail = coerce B.tail

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons = coerce B.uncons

last :: ByteString -> Word8
last = coerce B.last

init :: ByteString -> ByteString
init = coerce B.init

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc = coerce B.unsnoc

null :: ByteString -> Bool
null = coerce B.null

length :: ByteString -> Int64
length = primIntToInt64 . coerce B.length

map :: (Word8 -> Word8) -> ByteString -> ByteString
map = coerce B.map

reverse :: ByteString -> ByteString
reverse = coerce B.reverse

intersperse :: Word8 -> ByteString -> ByteString
intersperse = coerce B.intersperse

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate = coerce B.intercalate

transpose :: [ByteString] -> [ByteString]
transpose = coerce B.transpose

foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z x = B.foldl (coerce f) z (coerce x)

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f z x = B.foldl' (coerce f) z (coerce x)

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr f z x = B.foldr (coerce f) z (coerce x)

foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' f z x = B.foldr' (coerce f) z (coerce x)

foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 = coerce B.foldl1

foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' = coerce B.foldl1'

foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 = coerce B.foldr1

foldr1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' = coerce B.foldr1'

concat :: [ByteString] -> ByteString
concat = coerce B.concat

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap = coerce B.concatMap

any :: (Word8 -> Bool) -> ByteString -> Bool
any = coerce B.any

all :: (Word8 -> Bool) -> ByteString -> Bool
all = coerce B.all

maximum :: ByteString -> Word8
maximum = coerce B.maximum

minimum :: ByteString -> Word8
minimum = coerce B.minimum

compareLength :: ByteString -> Int64 -> Ordering
compareLength bs = compare (length bs)

mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f s x = coerce (B.mapAccumL (coerce f) s (coerce x))

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f s x = coerce (B.mapAccumR (coerce f) s (coerce x))

scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanl = coerce B.scanl

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 = coerce B.scanl1

scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanr = coerce B.scanr

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 = coerce B.scanr1

repeat :: Word8 -> ByteString
repeat = pack . List.repeat

replicate :: Int64 -> Word8 -> ByteString
replicate n = coerce (B.replicate (primInt64ToInt n))

cycle :: ByteString -> ByteString
cycle bs
  | null bs = error "Data.ByteString.Lazy.cycle: empty bytestring"
  | otherwise = bs' where bs' = bs `append` bs'

iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = pack . List.iterate f

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f a = coerce (B.unfoldr (coerce f) a)

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f a = coerce (B.unfoldrN n (coerce f) a)

take :: Int64 -> ByteString -> ByteString
take n = coerce B.take (primInt64ToInt n)

takeEnd :: Int64 -> ByteString -> ByteString
takeEnd n = coerce B.takeEnd (primInt64ToInt n)

drop :: Int64 -> ByteString -> ByteString
drop n = coerce B.drop (primInt64ToInt n)

dropEnd :: Int64 -> ByteString -> ByteString
dropEnd n = coerce B.dropEnd (primInt64ToInt n)

splitAt :: Int64 -> ByteString -> (ByteString, ByteString)
splitAt n = coerce B.splitAt (primInt64ToInt n)

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile = coerce B.takeWhile

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd = coerce B.takeWhileEnd

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile = coerce B.dropWhile

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd = coerce B.dropWhileEnd

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break = coerce B.break

breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd = coerce B.breakEnd

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span = coerce B.span

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd = coerce B.spanEnd

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith = coerce B.splitWith

split :: Word8 -> ByteString -> [ByteString]
split = coerce B.split

group :: ByteString -> [ByteString]
group = coerce B.group

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy = coerce B.groupBy

index :: ByteString -> Int64 -> Word8
index bs i = B.index (coerce bs) (primInt64ToInt i)

indexMaybe :: ByteString -> Int64 -> Maybe Word8
indexMaybe bs i = B.indexMaybe (coerce bs) (primInt64ToInt i)

(!?) :: ByteString -> Int64 -> Maybe Word8
(!?) = indexMaybe

elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex w = fmap primIntToInt64 . coerce B.elemIndex w

elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
elemIndexEnd w = fmap primIntToInt64 . coerce B.elemIndexEnd w

elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices w = fmap primIntToInt64 . coerce B.elemIndices w

count :: Word8 -> ByteString -> Int64
count w = primIntToInt64 . coerce B.count w

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex f = fmap primIntToInt64 . coerce B.findIndex f

findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndexEnd f = fmap primIntToInt64 . coerce B.findIndexEnd f

findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices f = fmap primIntToInt64 . coerce B.findIndices f

elem :: Word8 -> ByteString -> Bool
elem = coerce B.elem

notElem :: Word8 -> ByteString -> Bool
notElem = coerce B.notElem

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter = coerce B.filter

find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find = coerce B.find

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition = coerce B.partition

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf = coerce B.isPrefixOf

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix = coerce B.stripPrefix

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = coerce B.isSuffixOf

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix = coerce B.stripSuffix

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf = coerce B.isInfixOf

isValidUtf8 :: ByteString -> Bool
isValidUtf8 = coerce B.isValidUtf8

breakSubstring :: ByteString -> ByteString -> (ByteString,ByteString)
breakSubstring = coerce B.breakSubstring

zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip = coerce B.zip

zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f x y = B.zipWith (coerce f) (coerce x) (coerce y)

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith = coerce B.packZipWith

unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip = coerce B.unzip

inits :: ByteString -> [ByteString]
inits = coerce B.inits

initsNE :: ByteString -> NonEmpty ByteString
initsNE = coerce B.initsNE

tails :: ByteString -> [ByteString]
tails = coerce B.tails

tailsNE :: ByteString -> NonEmpty ByteString
tailsNE = coerce B.tailsNE

sort :: ByteString -> ByteString
sort = coerce B.sort

useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString x = B.useAsCString (coerce x)

useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen x = B.useAsCStringLen (coerce x)

packCString :: CString -> IO ByteString
packCString = coerce B.packCString

packCStringLen :: CStringLen -> IO ByteString
packCStringLen = coerce B.packCStringLen

copy :: ByteString -> ByteString
copy = coerce B.copy

getLine :: IO ByteString
getLine = coerce B.getLine

hGetLine :: Handle -> IO ByteString
hGetLine = coerce B.hGetLine

hPut :: Handle -> ByteString -> IO ()
hPut = coerce B.hPut

hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking = coerce B.hPutNonBlocking

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = coerce B.hPutStr

putStr :: ByteString -> IO ()
putStr = coerce B.putStr

hGet :: Handle -> Int -> IO ByteString
hGet = coerce B.hGet

hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = coerce B.hGetNonBlocking

hGetSome :: Handle -> Int -> IO ByteString
hGetSome = coerce B.hGetSome

hGetContents :: Handle -> IO ByteString
hGetContents = coerce B.hGetContents

getContents :: IO ByteString
getContents = coerce B.getContents

interact :: (ByteString -> ByteString) -> IO ()
interact = coerce B.interact

readFile :: FilePath -> IO ByteString
readFile = coerce B.readFile

writeFile :: FilePath -> ByteString -> IO ()
writeFile = coerce B.writeFile

appendFile :: FilePath -> ByteString -> IO ()
appendFile = coerce B.appendFile
