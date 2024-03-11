bin/mcabal:	src/MicroCabal/*.hs
	@mkdir -p bin
	ghc -outputdir ghc-out -Wall -Wno-unrecognised-warning-flags -Wno-x-partial --make -isrc -o bin/mcabal -main-is MicroCabal.Main MicroCabal.Main

clean:
	rm -rf ghc-out bin/*
	cabal clean

test:	bin/mcabal
	bin/mcabal MicroCabal.cabal
	bin/mcabal ../MicroHs/MicroHs.cabal
	bin/mcabal ../MicroHs/cpphssrc/malcolm-wallace-universe/polyparse-1.12/polyparse.cabal
	bin/mcabal ../MicroHs/cpphssrc/malcolm-wallace-universe/cpphs-1.20.9/cpphs.cabal
	bin/mcabal ..//Hackage/optparse-applicative/optparse-applicative.cabal
