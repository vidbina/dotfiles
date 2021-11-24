.PHONY: all clean test

HOME_MANAGER = home-manager

HOME_CONFIGURATION_FILE = ./home.nix

all:
	${HOME_MANAGER} build -f ${HOME_CONFIGURATION_FILE}

test:
	${HOME_MANAGER} build -f ${HOME_CONFIGURATION_FILE} -v -n
