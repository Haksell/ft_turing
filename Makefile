all: compile

compile:
	cabal build

run:
	@cabal run exe:ft_turing

test:
	@cabal test --test-show-details=direct

fclean:	
	cabal clean
	rm -f .ghc.environment*

re: fclean all

.PHONY: all compile run fclean re

