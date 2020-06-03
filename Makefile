# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: cacharle <marvin@42.fr>                    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2020/02/29 11:54:31 by cacharle          #+#    #+#              #
#    Updated: 2020/06/03 17:27:55 by charles          ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

GHC = ghc
RM = rm -f

GHCFLAGS = -dynamic --make -Wall

SRC_DIR = src
OBJ_DIR = build
NAME = computorv2

SRC = $(shell find $(SRC_DIR) -type f -name "*.hs")

all: $(NAME)

$(NAME): $(SRC)
	$(GHC) $(GHCFLAGS) -outputdir $(OBJ_DIR) -o $(NAME) $(SRC)

clean:
	$(RM) -r $(OBJ_DIR)

fclean: clean
	$(RM) $(NAME)

re: fclean all
