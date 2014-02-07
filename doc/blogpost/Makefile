.PHONY: svgs
svgs: eventlogs
	ghc-events-analyze -b 50 ex0-NT.eventlog
	ghc-events-analyze -b 50 ex0-N1.eventlog
	ghc-events-analyze -b 50 ex0-N2.eventlog
	ghc-events-analyze -b 50 ex1-NT.eventlog
	ghc-events-analyze -b 50 ex1-N1.eventlog
	ghc-events-analyze -b 50 ex1-N2.eventlog

.PHONY: eventlogs 
eventlogs: \
  ex0-NT.eventlog \
  ex0-N1.eventlog \
  ex0-N2.eventlog \
  ex1-NT.eventlog \
  ex1-N1.eventlog \
  ex1-N2.eventlog

ex0-NT.eventlog: ex0.hs
	ghc ex0 -eventlog -rtsopts -fforce-recomp -Wall
	./ex0 +RTS -l
	mv ex0.eventlog ex0-NT.eventlog

ex0-N1.eventlog: ex0.hs
	ghc ex0 -eventlog -rtsopts -fforce-recomp -threaded -Wall
	./ex0 +RTS -l -N1
	mv ex0.eventlog ex0-N1.eventlog

ex0-N2.eventlog: ex0.hs
	ghc ex0 -eventlog -rtsopts -fforce-recomp -threaded -Wall
	./ex0 +RTS -l -N2
	mv ex0.eventlog ex0-N2.eventlog

ex1-NT.eventlog: ex1.hs
	ghc ex1 -eventlog -rtsopts -fforce-recomp -Wall
	./ex1 +RTS -l
	mv ex1.eventlog ex1-NT.eventlog

ex1-N1.eventlog: ex1.hs
	ghc ex1 -eventlog -rtsopts -fforce-recomp -threaded -Wall
	./ex1 +RTS -l -N1
	mv ex1.eventlog ex1-N1.eventlog

ex1-N2.eventlog: ex1.hs
	ghc ex1 -eventlog -rtsopts -fforce-recomp -threaded -Wall
	./ex1 +RTS -l -N2
	mv ex1.eventlog ex1-N2.eventlog

.PHONY: clean
clean:
	rm -f ex*.eventlog ex*.svg
	rm -f *.o *.hi
	rm -f ex0 ex1 
	rm -f *.totals

# vi:set noexpandtab:
