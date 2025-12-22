SHELL = /usr/bin/env sh

HOME_MANAGER = home-manager
NIX_SHELL = nix-shell
DARWIN_REBUILD = sudo darwin-rebuild

.PHONY: rebuild

test:
	${NIX_SHELL} --command "make nix-build"

.PHONY: tangle
tangle:
	@echo "📝 Tangling org files..."
	@emacs --batch \
		--eval "(setq enable-local-variables :all)" \
		--eval "(setq enable-local-eval t)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(require 'org)" \
		--eval "(dolist (file '(\"README.org\" \"emacs/README.org\")) \
			(find-file file) \
			(hack-local-variables) \
			(org-babel-tangle) \
			(kill-buffer))"
	@echo "✅ Tangling complete"

# Check parity between tangled and committed files
.PHONY: check-parity
check-parity: tangle
	@echo "🔄 Checking parity..."
	@if git diff --exit-code; then \
		echo "✅ Parity check passed"; \
	else \
		echo "❌ Parity check failed - run 'make tangle' and commit"; \
		git diff; \
		exit 1; \
	fi

# Validate Nix configuration
.PHONY: validate
validate:
	@echo "🔍 Validating Nix configuration..."
	@nix flake check --no-build
	@nix build .#darwinConfigurations.berlin-4corei7.system --dry-run
	@echo "✅ Validation complete"


# Run all checks (CI simulation)
ci: check-parity validate
	@echo "✅ All CI checks passed"

.PHONY: nix-darwin-build
nix-darwin-build:
	${DARWIN_REBUILD} check --flake .#$(hostname)

.PHONY: nix-darwin-switch
nix-darwin-switch:
	${DARWIN_REBUILD} switch --flake .#$(hostname)

# Default target - show help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  make tangle            - Tangle all org files to generate config files"
	@echo "  make validate          - Validate Nix configuration without building"
	@echo "  make check-parity      - Check tangled files match committed files"
	@echo "  make ci                - Run all CI checks locally"
	@echo "  make nix-darwin-build  - Build nix-darwin config"
	@echo "  make nix-darwin-switch - Switch nix-darwin config"

.DEFAULT_GOAL := help
