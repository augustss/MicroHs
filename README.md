# Micro Haskell

MicroHs (`mhs`) is a compiler for an extended subset of Haskell 2010, with many GHC extensions. It compiles to combinators that are executed by a small, portable runtime system written in C. The compiler can compile itself, and all you need to build it is a C compiler; no Haskell compiler is required. The runtime is small enough to run on micro-controllers.

[Try it in your browser](https://augustss.github.io/MicroHs/web-mhs/).

## Installing

```
git clone --depth=1 https://github.com/augustss/MicroHs.git
cd MicroHs
make install
```
This installs `mhs`, `cpphs`, and `mcabal` (for installing packages) in `~/.mcabal/bin`, together with the compiled `base` package. Add `~/.mcabal/bin` to your `PATH`.

On Windows, make sure `cl` is in the path and use `nmake -f Makefile.windows install`.

## Example

The file `Example.hs` contains:
```haskell
module Example(fac, main) where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac(n - 1)

main :: IO ()
main = do
  let rs = map fac [1,2,3,10,52]
  putStrLn "Some factorials"
  print rs
```

Compile and run it:
```
$ mhs Example -oEx
$ ./Ex
Some factorials
[1,2,6,3628800,80658175170943878571660636856403766975289505440883277824000000000000]
```
Running `mhs` with no arguments starts an interactive session.

## Documentation

The full documentation is in [`doc/MicroHs.md`](doc/MicroHs.md).

There is also a [presentation from the Haskell Symposium 2024](https://docs.google.com/presentation/d/1WsSiSwypNVTm0oZ3spRYF8gA59wyteOsIkPRjCUAZec/edit?usp=sharing) ([video](https://m.youtube.com/watch?v=uMurx1a6Zck&t=36m)) and a short paper, [`doc/hs2024.pdf`](doc/hs2024.pdf).
