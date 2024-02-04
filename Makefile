all: compile

compile:
	cabal build

run:
	cabal run exe:ft-turing

fclean:	
	cabal clean

re: fclean all

.PHONY: all compile run fclean re