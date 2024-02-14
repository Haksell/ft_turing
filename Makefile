compile:
	cabal build

test:
	@cabal test --test-show-details=direct

fclean:	
	cabal clean
	rm -f .ghc.environment* .cabal-fmt* *.hi *.o dist-newstyle

re: fclean all

.PHONY: compile test fclean re

