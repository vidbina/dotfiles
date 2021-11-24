.PHONY: all clean test

HOME_MANAGER = home-manager

HOME_CONFIGURATION_FILE = ./home.nix

all:
	${HOME_MANAGER} -f ${HOME_CONFIGURATION_FILE} switch

test:
	${HOME_MANAGER} -f ${HOME_CONFIGURATION_FILE} -v -n build
