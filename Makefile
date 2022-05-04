SHELL = /usr/bin/env sh

.PHONY: all clean test

HOME_MANAGER = home-manager
NIX_SHELL = nix-shell

all:
	${NIX_SHELL} --command "make nix-switch"

test:
	${NIX_SHELL} --command "make nix-build"

nix-switch:
	${HOME_MANAGER} -f ${HOME_MANAGER_CONFIG} switch

nix-build:
	${HOME_MANAGER} -f ${HOME_MANAGER_CONFIG} -v -n build
