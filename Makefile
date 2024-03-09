bin/cabal:	src/MicroCabal/*.hs
	@mkdir -p bin
	ghc -Wall --make -isrc -i../MicroHs/src MicroCabal.Main -o bin/mcabal -main-is MicroCabal.Main

