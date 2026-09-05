module ConcatSymbol where
import Data.Proxy
import Data.TypeLits

testSuccess :: ConcatSymbol "ab" "cd" "abcd" => Bool
testSuccess = True

testConcat :: ConcatSymbol s1 s2 s3 => Proxy s1 -> Proxy s2 -> Proxy s3
testConcat _ _ = Proxy

testPrefix :: ConcatSymbol s1 s2 s3 => Proxy s3 -> Proxy s2 -> Proxy s1
testPrefix _ _ = Proxy

testSuffix :: ConcatSymbol s1 s2 s3 => Proxy s3 -> Proxy s1 -> Proxy s2
testSuffix _ _ = Proxy

testChain ::
  ( ConcatSymbol s1 s2 s3
  , ConcatSymbol s3 s4 s7)
  => Proxy s1 -> Proxy s2 -> Proxy s4 -> Proxy s7
testChain _ _ _ = Proxy

main = do
  putStrLn $ show testSuccess
  putStrLn $ symbolVal $ testConcat (Proxy :: Proxy "ab") (Proxy :: Proxy "cd")
  putStrLn $ symbolVal $ testPrefix (Proxy :: Proxy "abcd") (Proxy :: Proxy "cd")
  putStrLn $ symbolVal $ testSuffix (Proxy :: Proxy "abcd") (Proxy :: Proxy "ab")
  putStrLn $ symbolVal $
    testChain (Proxy :: Proxy "ab") (Proxy :: Proxy "cd") (Proxy :: Proxy "ef")
