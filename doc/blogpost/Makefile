# Blogpost examples
#
# See <https://www.well-typed.com/blog/2014/02/ghc-events-analyze/>.

.PHONY: svgs
svgs: eventlogs
# Examples from the blog post, in order.
	cabal run -- ghc-events-analyze -b 50 --script-timed nouser.script ex0-NT.eventlog
	cabal run -- ghc-events-analyze -b 50 ex1-NT.eventlog
	cabal run -- ghc-events-analyze -b 50 ex1-N1.eventlog
	cabal run -- ghc-events-analyze -b 50 ex1-N2.eventlog
# Window example (https://github.com/well-typed/ghc-events-analyze/pull/11)
	cabal run -- ghc-events-analyze -b 50 --script-timed nouser.script --window "WINDOW" ex0-windowed-NT.eventlog

.PHONY: eventlogs
eventlogs: \
  ex0-NT.eventlog \
  ex1-NT.eventlog \
  ex1-N1.eventlog \
  ex1-N2.eventlog \
  ex0-windowed-NT.eventlog

ex0-NT.eventlog:
	cabal run -- blogpost-examples-nt ex0 +RTS -l
	mv blogpost-examples-nt.eventlog ex0-NT.eventlog

ex1-NT.eventlog:
	cabal run -- blogpost-examples-nt ex1 +RTS -l
	mv blogpost-examples-nt.eventlog ex1-NT.eventlog

ex1-N1.eventlog:
	cabal run -- blogpost-examples-threaded ex1 +RTS -l -N1
	mv blogpost-examples-threaded.eventlog ex1-N1.eventlog

ex1-N2.eventlog:
	cabal run -- blogpost-examples-threaded ex1 +RTS -l -N2
	mv blogpost-examples-threaded.eventlog ex1-N2.eventlog

ex0-windowed-NT.eventlog:
	cabal run -- blogpost-examples-nt ex0-windowed +RTS -l
	mv blogpost-examples-nt.eventlog ex0-windowed-NT.eventlog

.PHONY: clean
clean:
	rm -f *.eventlog *.svg
	rm -f *.totals.txt *.timed.txt

# vi:set noexpandtab:
