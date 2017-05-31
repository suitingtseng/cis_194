all:
	ls *.hs | xargs -I{} ghc {}

clean:
	ls | grep -v 'Makefile' | grep -v '.hs' | xargs rm -f 
