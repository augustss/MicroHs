BIN=bin
BOOTDIR=ghc-boot
OUTDIR=ghc-out
TOOLS=Tools
PROF= #-prof -fprof-auto
EXTS= -XScopedTypeVariables -XQualifiedDo
GHCB=ghc $(PROF) -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude $(EXTS) -F -pgmF $(TOOLS)/convertY.sh 
GHCC=$(GHCB) $(GHCFLAGS)
GHC=ghc
# $(CURDIR) might not be quite right
GHCE=$(GHC) $(EXTS) -package mtl -F -pgmF Tools/convertX.sh -outputdir $(OUTDIR)
GCC=gcc
UPX=upx
ALLSRC=src/*/*.hs lib/*.hs lib/*/*.hs ghc/*.hs ghc/*/*.hs
MHS=mhs
COMB=comb/
EVAL=$(BIN)/eval
.PHONY: all alltest everytest boottest bootboottest bootcombtest $(MHS)test test alltest time example bootstraptest

all:	$(EVAL) $(BIN)/$(MHS)

alltest:	test boottest

everytest:	alltest example exampleboot examplecomb bootboottest bootcombtest

# On MINGW you might need the additional flags -Wl,--stack,50000000 to increase stack space.
$(EVAL):	src/runtime/eval.c
	@mkdir -p bin
	$(GCC) -Wall -O3 src/runtime/eval.c -o $(EVAL)

$(BIN)/$(MHS):	src/*.hs src/*/*.hs $(TOOLS)/convertX.sh
	$(GHCE) -isrc -Wall -O src/MicroHs/Main.hs -main-is MicroHs.Main -o $(BIN)/$(MHS)

$(BIN)/boot$(MHS):	$(ALLSRC) $(TOOLS)/convertY.sh
	rm -rf $(BOOTDIR)
	$(GHCB) -c ghc/Primitives.hs
	$(GHCB) -c ghc/Data/Bool_Type.hs
	$(GHCB) -c src/PrimTable.hs
	$(GHCC) -c lib/Control/Error.hs
	$(GHCC) -c lib/Data/Bool.hs
	$(GHCC) -c lib/Data/Int.hs
	$(GHCC) -c lib/Data/Char.hs
	$(GHCC) -c lib/Data/Either.hs
	$(GHCC) -c lib/Data/Tuple.hs
	$(GHCC) -c lib/Data/Function.hs
	$(GHCC) -c lib/Data/Maybe.hs
	$(GHCC) -c lib/Data/List.hs
	$(GHCC) -c lib/Text/String.hs
	$(GHCC) -c lib/Data/Word.hs
	$(GHCC) -c lib/System/IO.hs
	$(GHCC) -c lib/System/Environment.hs
	$(GHCC) -c lib/Prelude.hs
	$(GHCC) -c lib/PreludeNoIO.hs
	$(GHCC) -c lib/Data/Map.hs
	$(GHCC) -c lib/Data/IntMap.hs
	$(GHCC) -c lib/Unsafe/Coerce.hs
	$(GHCC) -c lib/Data/Integer.hs
	$(GHCC) -c lib/Control/Monad/State/Strict.hs
	$(GHCC) -c src/Text/ParserComb.hs
	$(GHCC) -c src/MicroHs/Ident.hs
	$(GHCC) -c src/MicroHs/Expr.hs
	$(GHCC) -c src/MicroHs/Lex.hs
	$(GHCC) -c src/MicroHs/Parse.hs
	$(GHCC) -c src/MicroHs/IdentMap.hs
	$(GHCC) -c src/MicroHs/StringMapFast.hs
#	$(GHCC) -c -package containers -package base src/MicroHs/StringMap.hs
	$(GHCC) -c src/MicroHs/Exp.hs
	$(GHCC) -c src/MicroHs/TCMonad.hs
	$(GHCC) -c src/MicroHs/TypeCheck.hs
	$(GHCC) -c src/MicroHs/Desugar.hs
	$(GHCC) -c src/MicroHs/StateIO.hs
	$(GHCC) -c src/MicroHs/Compile.hs
	$(GHCC) -c src/MicroHs/Translate.hs
	$(GHCC) -c -main-is MicroHs.Main src/MicroHs/Main.hs
	$(GHC) $(PROF) -hide-all-packages -package time -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*/*.o
#	$(GHC) $(PROF) -hide-all-packages -package containers -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*/*.o

# Test Haskell version with local libraries
boottest:	$(BIN)/boot$(MHS)
	$(BIN)/boot$(MHS) -ilib Example

# Compare version compiled with normal GHC libraries and $(MHS) libraries
bootboottest:	$(BIN)/$(MHS) $(BIN)/boot$(MHS)
	$(BIN)/$(MHS)     -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(BIN)/boot$(MHS) -ilib -isrc -omain-boot.comb MicroHs.Main
	cmp main-$(MHS).comb main-boot.comb

# Compare version compiled with GHC, and bootstrapped combinator version
bootcombtest:	$(BIN)/$(MHS) $(EVAL) $(COMB)$(MHS).comb
	$(BIN)/$(MHS) -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(EVAL) +RTS -v -r$(COMB)$(MHS).comb -RTS -ilib -isrc -omain-comb.comb MicroHs.Main
	cmp main-$(MHS).comb main-comb.comb

# Test normal Haskell version
test:	$(EVAL) $(BIN)/$(MHS) tests/*.hs
	cd tests; make test

$(COMB)$(MHS).comb:	$(BIN)/$(MHS) $(ALLSRC)
	$(BIN)/$(MHS) -ilib -isrc -o$(COMB)$(MHS).comb MicroHs.Main

$(MHS)comp:	$(EVAL) $(COMB)$(MHS).comb
	$(EVAL) +RTS -v -r$(COMB)$(MHS).comb -RTS $(ARG)

time:	$(EVAL) $(BIN)/$(MHS) tests/*.hs
	cd tests; make time

example:	$(EVAL) $(BIN)/$(MHS) Example.hs
	$(BIN)/$(MHS) -ilib Example && $(EVAL)

# does not work
exampleboot:	$(BIN)/boot$(MHS) Example.hs
	$(BIN)/boot$(MHS) -r -ilib Example && $(EVAL)

examplecomb:	$(EVAL) $(COMB)$(MHS).comb Example.hs
	$(EVAL) +RTS -r$(COMB)$(MHS).comb -RTS -r -ilib Example

clean:
	rm -rf src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out $(BOOTDIR) $(OUTDIR) tmp/eval.c Tools/*.o Tools/*.hi
	cd tests; make clean

#$(BIN)/addcombs:	Tools/Addcombs.hs
#	$(GHC) -main-is -make -iTools Addcomb.main Tools/Addcombs.hs -o $(BIN)/addcombs

tmp/eval.c: src/runtime/eval.c $(BIN)/eval
	@mkdir -p tmp
	cp src/runtime/eval.c tmp/eval.c
#	$(BIN)/addcombs $(COMB)$(MHS).comb >> tmp/eval.c
	$(BIN)/eval +RTS -K10M -r$(COMB)$(MHS).comb -RTS -ilib -iTools -r Addcombs -- $(COMB)$(MHS).comb >> tmp/eval.c

$(BIN)/cmhs: tmp/eval.c
	$(GCC) -Wall -O3 tmp/eval.c -o $(BIN)/cmhs
	strip $(BIN)/cmhs

$(BIN)/umhs: $(BIN)/cmhs
	$(UPX) -o$(BIN)/umhs $(BIN)/cmhs

# Test that the compiler can bootstrap
bootstraptest: $(EVAL)
	@mkdir -p tmp
	@echo Build stage 1 with distribution combinator file
	$(EVAL) +RTS -rcomb/mhs.comb  -RTS -ilib -isrc -otmp/mhs.comb.1 MicroHs.Main
	@echo Build stage 2 with output from stage 1
	$(EVAL) +RTS -rtmp/mhs.comb.1 -RTS -ilib -isrc -otmp/mhs.comb.2 MicroHs.Main
	cmp tmp/mhs.comb.1 tmp/mhs.comb.2 && echo Success
