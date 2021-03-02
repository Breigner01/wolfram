##
## EPITECH PROJECT, 2021
## B-FUN-400-PAR-4-1-wolfram-benjamin.reigner
## File description:
## Makefile
##

RM			?=	rm -f

STACKFLAGS	=	--system-ghc

NAME		=	wolfram

all:
	stack $(STACKFLAGS) build
	stack $(STACKFLAGS) --local-bin-path . install

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re
