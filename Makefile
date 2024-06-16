MHSDIR=../MicroHs
MHS=$(MHSDIR)/bin/mhs

bin/mcabal:	src/MicroCabal/*.hs src/MicroCabal/*/*.hs
	@mkdir -p bin
	MHSDIR=$(MHSDIR) $(MHS) -C -isrc -obin/mcabal MicroCabal.Main

bin/gmcabal:	src/MicroCabal/*.hs src/MicroCabal/*/*.hs
	@mkdir -p bin
	ghc -outputdir ghc-out -Wall -Wno-unrecognised-warning-flags -Wno-x-partial --make -isrc -o bin/gmcabal -main-is MicroCabal.Main -package directory -package process MicroCabal.Main

all:	bin/gmcabal bin/mcabal

install:: bin/mcabal
	@mkdir -p ~/.mcabal/bin
	cp bin/mcabal ~/.mcabal/bin

clean:
	rm -rf ghc-out bin/* .mhscache
	cabal clean

test:	bin/mcabal
	bin/mcabal parse MicroCabal.cabal
	bin/mcabal parse ../MicroHs/MicroHs.cabal
	bin/mcabal parse ../MicroHs/cpphssrc/malcolm-wallace-universe/polyparse-1.12/polyparse.cabal
	bin/mcabal parse ../MicroHs/cpphssrc/malcolm-wallace-universe/cpphs-1.20.9/cpphs.cabal
	bin/mcabal parse ../Hackage/optparse-applicative-0.18.1.0/optparse-applicative.cabal

install: bin/mcabal
	@mkdir -p ~/.mcabal/bin
	cp bin/mcabal ~/.mcabal/bin
