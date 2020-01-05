##
## EPITECH PROJECT, 2018
## MAKEFILE
## File description:
## Makefile
##

SRC	=   app/Main.hs

NAME	=	hal

all:	$(NAME)

$(NAME):	$(SRC)
		stack build --copy-bins --local-bin-path .

clean:
	rm -rf .stack-work hal.cabal
	rm -rf $(NAME)

fclean: clean
	$(RM) $(NAME)

re: fclean all
