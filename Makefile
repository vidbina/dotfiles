.PHONY: all clean test

HOME_MANAGER = home-manager

HOME_MANAGER_CONFIG ?= ./home.nix

all:
	${HOME_MANAGER} -f ${HOME_MANAGER_CONFIG} switch

test:
	${HOME_MANAGER} -f ${HOME_MANAGER_CONFIG} -v -n build
