# Define these 3 lines to use GMP for Integer.
#MHSGMPCCFLAGS=-DWANT_GMP=1
#MHSGMP=-ilib/gmp
#MCABALGMP=-fgmp
# AND, for MacOS with homebrew (after brew install gmp):
#MHSGMPCCLIBS= -L/opt/homebrew/lib -lgmp -I/opt/homebrew/include
# OR, for Ubuntu Linux (after apt-get install -y libgmp-dev):
#MHSGMPCCLIBS=-lgmp
#######################
#
# Unix-like system, 32/64 bit words
CONF=unix
#
RTS=src/runtime
RTSINC=-I$(RTS) -I$(RTS)/$(CONF)
MAINC= $(RTS)/main.c
#
MHSINCNOPATH= -i $(MHSGMP) -imhs -isrc -ilib
MHSINC=$(MHSINCNOPATH) -ipaths
MAINMODULE=MicroHs.Main
#
CCWARNS= -Wall
CCOPTS= -O3
CCLIBS= -lm $(MHSGMPCCLIBS)
CCSANITIZE= -fsanitize=undefined -fsanitize=address -fsanitize=pointer-compare -fsanitize=pointer-subtract
CCEVAL= $(CC) $(CCWARNS) $(CCOPTS) $(MHSGMPCCFLAGS) $(RTSINC) $(MAINC) $(RTS)/eval.c
#
MCABAL=$(HOME)/.mcabal
MCABALBIN=$(MCABAL)/bin
MCABALMHS=$(MCABAL)/mhs-$(VERSION)
MDATA=$(MCABALMHS)/packages/mhs-$(VERSION)/data
MRUNTIME=$(MDATA)/$(RTS)
MDIST=dist-mcabal
#
# For bootstrapping with GHC
GHC= ghc
GHCINCS= -ighc -isrc -ipaths
GHCWARNS= -Wall -Wno-unrecognised-warning-flags -Wno-x-partial -Wno-deprecations
GHCOPTS= -O
GHCEXTS= -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances
GHCPKGS= -package mtl -package pretty -package haskeline -package process -package time -package containers -package deepseq -package directory -package text -package bytestring -package filepath -package array
GHCOUT= -outputdir ghc-out
GHCPROF= # -prof -fprof-late #-prof -fprof-auto
GHCFLAGS= $(GHCEXTS) $(GHCINCS) $(GHCWARNS) $(GHCOPTS) $(GHCTOOL) $(GHCPKGS) $(GHCOUT) $(GHCPROF)
#
# Version numbers updated by the script updateversion.sh
VERSION=0.15.2.0
HVERSION=0,15,2,0
#


.PHONY: all install install_build inplace newmhs preparedist rmbinmhs newmhsz everytest everytestmhs

all:
	@echo "Use an explicit target.  Some common target:"
	@echo "  install             install in ~/.mcabal"
	@echo "  install_build       recompile and install in ~/.mcabal"
	@echo ""
	@echo "Targets for development:"
	@echo "  inplace             install for use in current directory"
	@echo "  newmhs              build bin/mhs using bin/gmhs"
	@echo "  preparedist         build everything needed for 'git push'"
	@echo "  clean               remove all build products"

targets.conf: targets.conf.in
	sed -e "s,GMPFLAGS,$(MHSGMPCCFLAGS)," -e "s,GMPLIBS,$(MHSGMPCCLIBS)," targets.conf.in > targets.conf

install: installmsg minstall

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

minstall: targets.conf
	@mkdir -p $(MCABALBIN)
	$(CCEVAL) generated/cpphs.c  $(CCLIBS) -o $(MCABALBIN)/cpphs
	$(CCEVAL) generated/mcabal.c $(CCLIBS) -o $(MCABALBIN)/mcabal
	$(CCEVAL) generated/mhs.c    $(CCLIBS) -o $(MCABALBIN)/mhs
	@mkdir -p $(MDATA)
	@mkdir -p $(MRUNTIME)
	cp targets.conf $(MDATA)
	cp -r $(RTS)/* $(MRUNTIME)
	$(MCABALBIN)/mhs -Q generated/base.pkg $(MCABALMHS)
	@echo $$PATH | tr ':' '\012' | grep -q $(MCABALBIN) || echo '***' Add $(MCABALBIN) to the PATH

preparedist: generated/cpphs.c generated/mcabal.c generated/mhs.c generated/base.pkg generated/mhseval.js bin/mhs
	bin/mhs -z $(MHSINC) $(MAINMODULE) -ogenerated/mhs_inplace.c  # generate the compressed version

generated/mcabal.c: bin/mhs
	bin/mhs -z -i../MicroCabal/src -ilib -ogenerated/mcabal.c MicroCabal.Main

generated/mhs.c: bin/mhs src/*/*.hs lib/*.hs lib/*/*.hs lib/*/*/*.hs $(MDIST)/Paths_MicroHs.hs
	bin/mhs -z $(MHSINCNOPATH) -i$(MDIST) $(MAINMODULE) -ogenerated/mhs.c

generated/base.pkg: bin/mhs bin/mcabal
	cd lib; MHS=../bin/mhs ../bin/mcabal $(MCABALGMP) build
	cp lib/$(MDIST)/base-$(VERSION).pkg generated/base.pkg

generated/mhs_inplace.c:	bin/gmhs src/*/*.hs lib/*.hs lib/*/*.hs lib/*/*/*.hs
	bin/gmhs $(MHSINC) $(MAINMODULE) -ogenerated/mhs_inplace.c

$(MDIST)/Paths_MicroHs.hs:
	@mkdir -p $(MDIST)
	@echo 'module Paths_MicroHs where'                                 > $(MDIST)/Paths_MicroHs.hs
	@echo 'import qualified Prelude(); import MHSPrelude'             >> $(MDIST)/Paths_MicroHs.hs
	@echo 'import Data.Version; import System.Directory'              >> $(MDIST)/Paths_MicroHs.hs
	@echo 'version :: Version; version = makeVersion [$(HVERSION)]'   >> $(MDIST)/Paths_MicroHs.hs
	@echo 'getDataDir :: IO FilePath'                                 >> $(MDIST)/Paths_MicroHs.hs
	@echo 'getDataDir = (++ "/packages/mhs-$(VERSION)/data") `fmap` getHomeDirectory'  >> $(MDIST)/Paths_MicroHs.hs

############################ bin/mhs ############################
bin/mhs: $(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h #generated/mhs_inplace.c
	@mkdir -p bin
	$(CCEVAL) generated/mhs_inplace.c $(CCLIBS) -o bin/mhs

############################ bin/cpphs ############################
bin/cpphs:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h #generated/cpphs.c
	@mkdir -p bin
	$(CCEVAL) generated/cpphs.c $(CCLIBS) -o bin/cpphs

############################ bin/mcabal ############################
bin/mcabal:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h #generated/mcabal.c
	@mkdir -p bin
	$(CCEVAL) generated/mcabal.c $(CCLIBS) -o bin/mcabal

############################ bin/gmhs ############################
# Compile mhs with ghc
bin/gmhs:	src/*/*.hs ghc/*.hs ghc/*/*.hs ghc/*/*/*.hs
	@mkdir -p bin
	$(GHC) $(GHCFLAGS) -main-is $(MAINMODULE) $(MAINMODULE) -o bin/gmhs

############################ generated/cpphs.c ############################
# Fetch cpphs submodule
cpphssrc/malcolm-wallace-universe/.git:
	git submodule update --init --depth 1 cpphssrc/malcolm-wallace-universe

# Use this cpphs for bootstrapping
USECPPHS=bin/cpphs

generated/cpphs.c: bin/mhs cpphssrc/malcolm-wallace-universe/.git
	MHSCPPHS=$(USECPPHS) bin/mhs -z -XCPP '-DMIN_VERSION_base(x,y,z)=((x)<4||(x)==4&&(y)<19||(x)==4&&(y)==19&&(z)<=1)' -icpphscompat -icpphssrc/malcolm-wallace-universe/polyparse-1.12/src -icpphssrc/malcolm-wallace-universe/cpphs-1.20.9 cpphssrc/malcolm-wallace-universe/cpphs-1.20.9/cpphs.hs -ogenerated/cpphs.c

############################ generated/mhseval.js ############################
# mhseval, compiled with emscripten. Use in the browser!
EMCCOPTS= -O3 -sEXPORTED_FUNCTIONS=_apply_sp,_main -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE -DUSE_SYSTEM_RAW -Wno-address-of-packed-member
EMCCLIBS= -lm
EMCCEVAL= emcc $(EMCCOPTS) $(RTSINC) $(MAINC) $(RTS)/eval.c
generated/mhseval.js:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h
	@mkdir -p bin
	$(EMCCEVAL) $(RTS)/comb.c $(EMCCLIBS) -o generated/mhseval.js

############################ newmhs ############################
newmhs: rmbinmhs generated/mhs_inplace.c bin/mhs

rmbinmhs:
	rm -f bin/mhs generated/mhs_inplace.c

newmhsz:
	@echo "Use target preparedist to prepare distribution"

############################ clean ############################
clean:
	rm -rf src/*/*.hi src/*/*.o *.comb *.js *.tmp *~ bin/* a.out $(GHCOUTDIR) Tools/*.o Tools/*.hi dist-newstyle generated/*-stage* .mhscache .mhscache dist-mcabal Interactive.hs .mhsi lib/*.pkg lib/dist-mcabal targets.conf
	cd tests; $(MAKE) clean
	-cabal clean
	-git submodule deinit cpphssrc/malcolm-wallace-universe
	-git submodule deinit MicroCabal

############################ everytest ############################

everytest:	newmhs bin/cpphs bin/mhseval targets.conf exampletest info runtest cachetest bootcombtest nfibtest

everytestmhs:	bin/mhs bin/cpphs bin/mhseval targets.conf exampletest info cachetest bootstrap runtestmhs nfibtest

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

# Run test examples with ghc-compiled compiler
runtest:	bin/mhseval bin/gmhs tests/*.hs
	cd tests; $(MAKE) alltest

# Run test examples with mhs-compiled compiler
runtestmhs: bin/mhseval bin/mhs
	cd tests; $(MAKE) MHS=../bin/mhs cache; $(MAKE) MHS="../bin/mhs +RTS -H4M -RTS -CR" alltest

# Compile combinator evaluator
bin/mhseval:	$(RTS)/*.c $(RTS)/*.h $(RTS)/*/*.h
	@mkdir -p bin
	$(CCEVAL) $(RTS)/comb.c $(CCLIBS) -o bin/mhseval

############################ inplace ############################
inplace: bin/mhs bin/cpphs bin/mcabal bin/mhseval

############################ bootstrap ############################

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

