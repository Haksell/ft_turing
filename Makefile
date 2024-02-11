compile:
	cabal build

run:
	@cabal run -v0 exe:ft_turing configurations/unary_sub.json '111-11='

test:
	@cabal test --test-show-details=direct

fclean:	
	cabal clean
	rm -f .ghc.environment* .cabal-fmt* *.hi *.o

re: fclean all

.PHONY: all compile run fclean re

