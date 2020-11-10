##
## EPITECH PROJECT, 2020
## FUN_imageCompressor_2019
## File description:
## Makefile
##

package			=	FunHAL
NAME			=	hal

stack_yaml		=	STACK_YAML="stack.yaml"
stack			=	$(stack_yaml) stack --allow-different-user
executable		:=	`stack path --local-install-root`


all: build sign

re: fclean all

clean:
	@echo "Cleaning Stack Project"


fclean: clean
	$(RM) $(package) $(NAME)


build:
	$(stack) build $(package)
	@cp $(executable)/bin/$(NAME) .

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)


build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build


run:
	$(stack) build --fast && $(stack) exec -- $(package)


install:
	$(stack) install


ghci:
	$(stack) ghci $(package):lib --ghci-options='-j6 +RTS -A128m'


test:
	-$(stack) test $(package)


test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'


bench:
	$(stack) bench $(package)


ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind -j4 +RTS -A128m' --main-is $(package):$(package)"


dev-deps:
	$(stack) install ghcid

func_tests:
	-tests/jenrik/all.sh

tests_run: re test func_tests

sign:
	@echo ""
	@echo "*******************"
	@echo "* Damien Bernard  *"
	@echo "*                 *"
	@echo "*  Epitech 2020   *"
	@echo "*******************"
	@echo ""

.PHONY : build build-dirty run install ghci test func_tests test-ghci ghcid dev-deps re clean fclean all
