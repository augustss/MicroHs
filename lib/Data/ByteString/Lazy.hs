-- MicroHs does not distinguish lazy and strict bytestrings
module Data.ByteString.Lazy(module Data.ByteString.Lazy) where
import qualified Data.ByteString as B
import Data.Coerce
import Data.List.NonEmpty
import Data.Word.Word8(Word8)
import Foreign.C.String(CString, CStringLen)
import System.IO.Base(Handle)

newtype ByteString = BS B.ByteString
  deriving (Eq, Ord)

instance IsString ByteString where
  fromString = pack

pack :: String -> ByteString
pack = coerce B.pack

unpack :: ByteString -> String
unpack = coerce B.unpack

fromFilePath :: FilePath -> IO ByteString
fromFilePath = coerce B.fromFilePath

toFilePath :: ByteString -> IO FilePath
toFilePath = coerce B.toFilePath

cons :: Word8 -> ByteString -> ByteString
cons = coerce B.cons

snoc :: ByteString -> Word8 -> ByteString
snoc = coerce B.snoc

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

map :: (Word8 -> Word8) -> ByteString -> ByteString
map = coerce B.map

reverse :: ByteString -> ByteString
reverse = coerce B.reverse

intersperse :: Word8 -> ByteString -> ByteString
intersperse = coerce B.intersperse

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

mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f s x = coerce (B.mapAccumL (coerce f) s (coerce x))

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f s x = coerce (B.mapAccumR (coerce f) s (coerce x))

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 = coerce B.scanl1

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 = coerce B.scanr1

replicate :: Int -> Word8 -> ByteString
replicate = coerce B.replicate

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f a = coerce (B.unfoldr (coerce f) a)

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f a = coerce (B.unfoldrN n (coerce f) a)

take :: Int -> ByteString -> ByteString
take = coerce B.take

takeEnd :: Int -> ByteString -> ByteString
takeEnd = coerce B.takeEnd

drop  :: Int -> ByteString -> ByteString
drop = coerce B.drop

dropEnd :: Int -> ByteString -> ByteString
dropEnd = coerce B.dropEnd

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt = coerce B.splitAt

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

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate = coerce B.intercalate

index :: ByteString -> Int -> Word8
index = coerce B.index

indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe = coerce B.indexMaybe

(!?) :: ByteString -> Int -> Maybe Word8
(!?) = coerce (B.!?)

elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex = coerce B.elemIndex

elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd = coerce B.elemIndexEnd

elemIndices :: Word8 -> ByteString -> [Int]
elemIndices = coerce B.elemIndices

count :: Word8 -> ByteString -> Int
count = coerce B.count

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex = coerce B.findIndex

findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndexEnd = coerce B.findIndexEnd

findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices = coerce B.findIndices

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
