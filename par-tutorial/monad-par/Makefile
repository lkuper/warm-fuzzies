default:
	make basics

basics:
	ghc -O2 basics.hs -rtsopts -threaded -eventlog

runbasics:
	make basics
	./basics +RTS -N -ls
	~/.cabal/bin/threadscope basics.eventlog

clean:
	rm *.eventlog *.hi *.o

