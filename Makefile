help:
	@cabal run -v0 exe:ft_turing -- --help

test:
	@cabal test --test-show-details=direct

clean:	
	cabal clean
	rm -f .ghc.environment* .cabal-fmt* *.hi *.o dist-newstyle

.PHONY: test clean

