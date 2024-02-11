compile:
	cabal build

run:
	@cabal run exe:ft_turing configurations/unary_sub.json '111-11='

test:
	@cabal test --test-show-details=direct

fclean:	
	cabal clean
	rm -f .ghc.environment* *.cabal-fmt*

re: fclean all

.PHONY: all compile run fclean re

