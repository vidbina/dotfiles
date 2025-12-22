SHELL = /usr/bin/env sh

HOME_MANAGER = home-manager
NIX_SHELL = nix-shell

.PHONY: all clean test

all:
	${NIX_SHELL} --command "make nix-switch"

test:
	${NIX_SHELL} --command "make nix-build"

nix-switch:
	${HOME_MANAGER} switch -f ${HOME_MANAGER_CONFIG} --show-trace

nix-build:
	${HOME_MANAGER} build -f ${HOME_MANAGER_CONFIG} -n --show-trace -v
