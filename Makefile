bin/cabal:	src/MicroCabal/*.hs
	@mkdir -p bin
	ghc -outputdir ghc-out -Wall -Wno-unrecognised-warning-flags -Wno-x-partial --make -isrc -o bin/mcabal -main-is MicroCabal.Main MicroCabal.Main

clean:
	rm -rf ghc-out bin/*
	cabal clean
