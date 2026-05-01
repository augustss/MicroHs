# Define these 3 lines to use GMP for Integer.
#MHSGMPCCFLAGS=-DWANT_GMP=1
#MHSGMP=-ilib/gmp
#MCABALGMP=-fgmp
# AND, for MacOS with homebrew (after brew install gmp):
#MHSGMPCCLIBS= -L/opt/homebrew/lib -lgmp -I/opt/homebrew/include
# OR, for Ubuntu Linux (after apt-get install -y libgmp-dev):
#MHSGMPCCLIBS=-lgmp
#
# installation prefix
PREFIX=/usr/local
# Unix-like system, 32/64 bit words
CONF=unix
#
# Using GCC enables global register variables on ARM64, which gives a 5-10% speedup.
#CC=gcc-14
RTS=src/runtime
RTSINC=-I$(RTS) -I$(RTS)/$(CONF)
MAINC= $(RTS)/main.c
#
CCWARNS= -Wall
CCOPTS= -O3
CCLIBS= -lm $(MHSGMPCCLIBS)
CCSANITIZE= -fsanitize=undefined -fsanitize=address -fsanitize=pointer-compare -fsanitize=pointer-subtract
CCEVAL= $(CC) $(CCWARNS) $(CCOPTS) $(MHSGMPCCFLAGS) $(RTSINC) $(MAINC) $(RTS)/eval.c
#
GHC= ghc
GHCINCS= -ighc -isrc
GHCWARNS= -Wall -Wno-unrecognised-warning-flags -Wno-x-partial -Wno-deprecations
GHCOPTS= -O
GHCEXTS= -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances
GHCPKGS= -package mtl -package pretty -package haskeline -package process -package time -package ghc-prim -package containers -package deepseq -package directory -package text -package bytestring -package filepath -package array
GHCTOOL= # -F -pgmF Tools/convertX.sh
GHCOUTDIR= ghc-out
GHCOUT= -outputdir $(GHCOUTDIR)
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF) -main-is MicroHs.Main
#
# Hugs
HUGS= runhugs
FFIHUGS= ffihugs
HUGSINCS= '+Phugs:mhs:src:{Hugs}/packages/*:hugs/obj' -98 +o +w

#
NODE=node
#NODEFLAGS=--stack_size=8192
#
MHSINCNP= -i $(MHSGMP) -imhs -isrc -ilib
MHSINC=$(MHSINCNP)
MAINMODULE=MicroHs.Main
#
.PHONY:	clean bootstrap install ghcgen newmhs newmhsz cachelib timecompile exampletest cachetest runtest runtestmhs everytest everytestmhs nfibtest info install minstall installmsg

all:	bin/mhs bin/cpphs bin/mcabal

newmhs:	ghcgen mhs.conf
	$(CCEVAL) generated/mhs.c $(CCLIBS) -o bin/mhs
	$(CC) $(CCWARNS) $(MHSGMPCCFLAGS) -g $(RTSINC) $(RTS)/eval.c $(MAINC) generated/mhs.c $(CCLIBS) -o bin/mhsgdb

newmhsz:	newmhs
	rm generated/mhs.c
	$(MAKE) generated/mhs.c

sanitizemhs:	ghcgen mhs.conf
	$(CCEVAL) $(CCSANITIZE) generated/mhs.c $(CCLIBS) -o bin/mhssane

# Compile mhs from distribution, with C compiler
bin/mhs:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h mhs.conf #generated/mhs.c
	@mkdir -p bin
	$(CCEVAL) generated/mhs.c $(CCLIBS) -o bin/mhs

# Compile cpphs from distribution, with C compiler
bin/cpphs:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h #generated/cpphs.c
	@mkdir -p bin
	$(CCEVAL) generated/cpphs.c $(CCLIBS) -o bin/cpphs

# Compile mcabal from distribution, with C compiler
bin/mcabal:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h #generated/mcabal.c
	@mkdir -p bin
	$(CCEVAL) generated/mcabal.c $(CCLIBS) -o bin/mcabal

# Compile combinator evaluator
bin/mhseval:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h
	@mkdir -p bin
	$(CCEVAL) $(RTS)/comb.c $(CCLIBS) -o bin/mhseval
	size bin/mhseval

bin/mhsevalgdb:	$(RTS)/*.c $(RTS)/*/*.h
	@mkdir -p bin
	$(CC) $(CCWARNS) $(MHSGMPCCFLAGS) $(RTSINC) -g $(RTS)/eval.c $(RTS)/comb.c $(MAINC) $(CCLIBS) -o bin/mhsevalgdb

bin/mhsevalsane:	$(RTS)/*.c $(RTS)/*/*.h
	@mkdir -p bin
	$(CCEVAL) $(CCSANITIZE) $(RTSINC) $(RTS)/comb.c $(CCLIBS) -o bin/mhsevalsane

#######################
# mhseval, compiled with emscripten. Use in the browser!
#EMCC=emcc
#EMCCFLAGS=-O3 -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sNODERAWFS -sSINGLE_FILE -DUSE_SYSTEM_RAW -sEXIT_RUNTIME
EMCCOPTS= -O3 -sEXPORTED_FUNCTIONS=_apply_sp,_main -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE -DUSE_SYSTEM_RAW -Wno-address-of-packed-member
EMCCLIBS= -lm
EMCCEVAL= emcc $(EMCCOPTS) $(RTSINC) $(MAINC) $(RTS)/eval.c
generated/mhseval.js:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h
	@mkdir -p bin
	$(EMCCEVAL) $(RTS)/comb.c $(EMCCLIBS) -o generated/mhseval.js

bin/mhs.js:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h mhs.conf #generated/mhs.c
	@mkdir -p bin
	$(EMCCEVAL) generated/mhs.c $(EMCCLIBS) -o bin/mhs.js

#######################
# Compile mhs with ghc
bin/gmhs:	src/*/*.hs ghc/*.hs ghc/*/*.hs ghc/*/*/*.hs
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) $(RTSINC) $(MAINMODULE) -o bin/gmhs

# Compile mhs with ghc, with code coverage
bin/cmhs:	src/*/*.hs ghc/*.hs ghc/*/*.hs
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) $(RTSINC) -fhpc $(MAINMODULE) -o bin/cmhs

# Generate distribution C file
generated/mhs.c:	bin/mhs src/*/*.hs
	@mkdir -p generated
	bin/mhs -z $(MHSINC) $(MAINMODULE) -ogenerated/mhs.c

ghcgen:	bin/gmhs src/*/*.hs lib/*.hs lib/*/*.hs lib/*/*/*.hs
	bin/gmhs $(MHSINC) $(MAINMODULE) -ogenerated/mhs.c

# This doesn't keep MicroCabal updated.  I'm not sure how to do it
#generated/mcabal.c: MicroCabal/.git
#	bin/mhs -z -iMicroCabal/src -ilib -ogenerated/mcabal.c MicroCabal.Main
#
#MicroCabal/.git:
#	git submodule update --init --depth 1 MicroCabal
generated/mcabal.c:
	bin/mhs -z -i../MicroCabal/src -ilib -ogenerated/mcabal.c MicroCabal.Main

# Flags to read local file system, generate a single .js file, and to avoid ioctl()
mhs.js:	src/*/*.hs $(RTS)/*.h $(RTS)/*/*.h mhs.conf
	bin/mhs $(MHSINC) -temscripten $(MAINMODULE) -o mhs.js

# Make sure boottrapping works
bootstrap:	bin/mhs-stage2
	@echo "*** copy stage2 to bin/mhs"
	cp bin/mhs-stage2 bin/mhs
	cp generated/mhs-stage2.c generated/mhs.c

# Build stage1 compiler with existing compiler
bin/mhs-stage1:	bin/mhs src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage1 compiler, using bin/mhs"
	bin/mhs -z $(MHSINC) $(MAINMODULE) -ogenerated/mhs-stage1.c
	$(CCEVAL) generated/mhs-stage1.c $(CCLIBS) -o bin/mhs-stage1

# Build stage2 compiler with stage1 compiler, and compare
bin/mhs-stage2:	bin/mhs-stage1 src/*/*.hs
	@mkdir -p generated
	@echo "*** Build stage2 compiler, with stage1 compiler"
	bin/mhs-stage1 -z $(MHSINC) $(MAINMODULE) -ogenerated/mhs-stage2.c
	cmp generated/mhs-stage1.c generated/mhs-stage2.c
	@echo "*** stage2 equal to stage1"
	$(CCEVAL) generated/mhs-stage2.c $(CCLIBS) -o bin/mhs-stage2

# Fetch cpphs submodule
cpphssrc/malcolm-wallace-universe/.git:
	git submodule update --init --depth 1 cpphssrc/malcolm-wallace-universe

# Use this cpphs for bootstrapping
USECPPHS=bin/cpphs

bootstrapcpphs: bin/mhs cpphssrc/malcolm-wallace-universe/.git
	MHSCPPHS=$(USECPPHS) bin/mhs -z -XCPP '-DMIN_VERSION_base(x,y,z)=((x)<4||(x)==4&&(y)<19||(x)==4&&(y)==19&&(z)<=1)' -icpphscompat -icpphssrc/malcolm-wallace-universe/polyparse-1.12/src -icpphssrc/malcolm-wallace-universe/cpphs-1.20.9 cpphssrc/malcolm-wallace-universe/cpphs-1.20.9/cpphs.hs -ogenerated/cpphs.c

mhs.conf: mhs.conf.in
	sed -e "s,GMPFLAGS,$(MHSGMPCCFLAGS)," -e "s,GMPLIBS,$(MHSGMPCCLIBS)," mhs.conf.in > mhs.conf

# Run test examples with ghc-compiled compiler
runtest:	bin/mhseval bin/gmhs tests/*.hs
	cd tests; $(MAKE) alltest

# Run test examples with mhs-compiled compiler
runtestmhs: bin/mhseval bin/mhs
	cd tests; $(MAKE) MHS=../bin/mhs cache; $(MAKE) MHS="../bin/mhs +RTS -H4M -RTS -CR" alltest interactivetest

runitestmhs: bin/mhseval bin/mhs
	cd tests; $(MAKE) MHS=../bin/mhs cache; $(MAKE) MHS="../bin/mhs +RTS -H4M -RTS -CR" interactivetest

# Run test examples with sanitized mhs-compiled compiler
runtestsan: bin/mhsevalsane sanitizemhs
	cd tests; $(MAKE) MHS="../bin/mhssane +RTS -H4M -RTS -CW" cache
	cd tests; $(MAKE) MHS="../bin/mhssane +RTS -H4M -RTS -CR" EVAL="../bin/mhsevalsane +RTS -H1M -RTS" info test errtest

runtestgsan: bin/mhsevalsane bin/gmhs
	cd tests; $(MAKE) EVAL="../bin/mhsevalsane +RTS -H1M -RTS" info test errtest

# Run test examples going via JavaScript
runtestemscripten: bin/mhseval bin/mhs bin/cpphs
	cd tests; $(MAKE) MHS=../bin/mhs cache; MHSDIR=.. $(MAKE) MHSTARGET="-temscripten" MHS="../bin/mhs -CR -oout.js" EVAL="$(NODE) $(NODEFLAGS) out.js" info test errtest

# Run test examples going with tcc
runtesttcc: bin/mhseval bin/mhs bin/cpphs
	cd tests; MHSDIR=.. $(MAKE) MHSTARGET="-ttcc" MHSOUTPUT="-oa.out" EVAL="./a.out" info test errtest


# Compress the binary (broken on MacOS)
bin/umhs: bin/mhs
	rm -f bin/umhs
	upx -q -q -obin/umhs bin/mhs

#
timecompile: bin/mhs
	@date
	@git rev-parse HEAD
	time bin/mhs +RTS -v -RTS $(MHSINC) $(MAINMODULE)

#
timecachecompile: bin/mhs
	@-rm -f .mhscache
	time bin/mhs +RTS -v -RTS -CW AllOfLib
	time bin/mhs +RTS -v -RTS -CR -s $(MHSINC) $(MAINMODULE)

#
timemhscompile:
	@date
	@git rev-parse HEAD
	time mhs +RTS -v -RTS -z -imhs -isrc $(MAINMODULE)

timegmhscompile:
	@date
	@git rev-parse HEAD
	time bin/gmhs -imhs -isrc $(MAINMODULE)

timeghccompile:
	@date
	@git rev-parse HEAD
	time $(GHC) -fforce-recomp $(GHCFLAGS) -O0 $(MAINMODULE) -o bin/gmhs

#
cachelib:
	@-rm -f .mhscache
	bin/mhs -CW AllOfLib

#
clean:
	rm -rf src/*/*.hi src/*/*.o *.comb *.js *.tmp *~ bin/* a.out $(GHCOUTDIR) Tools/*.o Tools/*.hi dist-newstyle generated/*-stage* .mhscache .mhscache dist-mcabal Interactive.hs .mhsi lib/*.pkg lib/dist-mcabal mhs.conf
	cd tests; $(MAKE) clean
	-cabal clean
	-git submodule deinit cpphssrc/malcolm-wallace-universe
	-git submodule deinit MicroCabal

oldinstall:
	mkdir -p $(PREFIX)/bin
	cp bin/mhs $(PREFIX)/bin
	-cp bin/cpphs $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/mhs/$(RTS)
	cp -r lib $(PREFIX)/lib/mhs
	cp -r $(RTS)/* $(PREFIX)/lib/mhs/$(RTS)
	cp mhs.conf $(PREFIX)/lib/mhs/mhs.conf
	@echo "***"
	@echo "*** Installation complete"
	@echo "*** Set environment variable MHSDIR to $(PREFIX)/lib/mhs"
	@echo "***"

everytest:	newmhs bin/cpphs exampletest info runtest cachetest bootcombtest nfibtest

everytestmhs:	bin/mhs bin/cpphs bin/mhseval exampletest info cachetest bootstrap runtestmhs nfibtest

bootcombtest:	bin/gmhs bin/mhseval
	bin/gmhs $(MHSINC) -ogmhs.comb $(MAINMODULE)
	bin/mhseval +RTS -v -rgmhs.comb -RTS $(MHSINC) -omhs.comb $(MAINMODULE)
	cmp gmhs.comb mhs.comb

exampletest:	bin/mhs bin/mhseval Example.hs
	bin/mhs -r Example
	bin/mhs Example && bin/mhseval
	bin/mhs Example -oEx && ./Ex && rm Ex

examplejs: bin/mhs Example.hs
	bin/mhs -temscripten Example -oEx.js
	$(NODE) $(NODEFLAGS) Ex.js
	rm Ex.js

info:	bin/mhs
	bin/mhs -r -itests Info

cachetest:	bin/mhs bin/cpphs bin/mhseval Example.hs
	rm -f .mhscache
	bin/mhs -CW AllOfLib
	bin/mhs -CR Example && bin/mhseval
	bin/mhs +RTS -v -RTS $(MHSINC) -CR $(MAINMODULE)
	rm -f .mhscache

nfibtest: bin/mhs bin/mhseval
	bin/mhs -itests Nfib && bin/mhseval

######

VERSION=0.15.8.0
HVERSION=0,15,8,0
MCABAL=$(HOME)/.mcabal
MCABALMHS=$(MCABAL)/mhs-$(VERSION)
MDATA=$(MCABALMHS)/packages/mhs-$(VERSION)/data
MRUNTIME=$(MDATA)/$(RTS)
MCABALBIN=$(MCABAL)/bin
BASE=base-$(VERSION)
$(MCABALBIN)/mhs: bin/mhs $(RTS)/*.[ch] mhs.conf machdep
	@mkdir -p $(MCABALBIN)
	bin/mhs -z $(MHSINCNP) $(MAINMODULE) -o$(MCABALBIN)/mhs

$(MCABALBIN)/cpphs: bin/cpphs
	@mkdir -p $(MCABALBIN)
	cp bin/cpphs $(MCABALBIN)

$(MCABALBIN)/mcabal: bin/mcabal
	@mkdir -p $(MCABALBIN)
	cp bin/mcabal $(MCABALBIN)

preparedist:	newmhsz bootstrapcpphs
	rm -f generated/mcabal.c generated/mhseval.js generated/base.pkg
	$(MAKE) generated/mcabal.c
	$(MAKE) generated/mhseval.js
	$(MAKE) generated/base.pkg

generated/base.pkg: bin/mhs bin/mcabal
	cd lib; PATH=../bin:"$$PATH" mcabal $(MCABALGMP) build
	cp lib/dist-mcabal/base-$(VERSION).pkg generated/base.pkg

install:	installmsg minstall

installmsg:
	@echo '***************************************************'
	@echo "* Installing MicroHs $(VERSION)"
	@echo "*  Binaries (in $(MCABALBIN)):"
	@echo '*    mhs     - MicroHs compiler'
	@echo '*    cpphs   - C preprocessor'
	@echo '*    mcabal  - cabal for MicroHs'
	@echo '*  Libraries:'
	@echo '*    base (bytestring, deepseq, directory, hashable, text, stm)'
	@echo '***************************************************'
	@echo ''

minstall:	bin/mhs bin/cpphs bin/mcabal machdep
	@mkdir -p $(MCABALBIN)
	@mkdir -p $(MRUNTIME)
	cp bin/mhs bin/cpphs bin/mcabal $(MCABALBIN)
	cp mhs.conf $(MDATA)
	cp -r $(RTS)/* $(MRUNTIME)
	@mkdir -p $(MCABALMHS)
	bin/mhs -Q generated/base.pkg $(MCABALMHS)
	@echo $$PATH | tr ':' '\012' | grep -q $(MCABALBIN) || echo '***' Add $(MCABALBIN) to the PATH

machdep:
	$(CC) Tools/machdep.c -o machdep.exe && ./machdep.exe > $(RTS)/MachDeps.h && rm machdep.exe

# Install with recompiling
slowinstall:	bin/cpphs bin/mcabal bin/mhs machdep
	@mkdir -p $(MCABALBIN)
	@mkdir -p $(MRUNTIME)
	cp bin/cpphs bin/mcabal $(MCABALBIN)
	cp mhs.conf $(MDATA)
	cp -r $(RTS)/* $(MRUNTIME)
	bin/mhs -z $(MHSINCNP) $(MAINMODULE) -o$(MCABALBIN)/mhs
	cd lib; PATH=$(MCABALBIN):"$$PATH" mcabal $(MCABALGMP) install
	@echo $$PATH | tr ':' '\012' | grep -q $(MCABALBIN) || echo '***' Add $(MCABALBIN) to the PATH

#####
# Hugs
HUGS= runhugs
HUGSINCS= '+Phugs:src:{Hugs}/packages/*:hugs/obj' -98 +o +w -h100m

generated/hmhs.c:
	@mkdir -p generated
	$(HUGS) $(HUGSINCS) hugs/Main.hs $(MHSINC) $(MAINMODULE) -ogenerated/hmhs.c

bin/hmhs: generated/hmhs.c
	@mkdir -p bin
	$(CCEVAL) generated/hmhs.c $(CCLIBS) -o bin/hmhs

