SHELL = /usr/bin/env sh

HOME_MANAGER = home-manager
NIX_SHELL = nix-shell
DARWIN_REBUILD = sudo darwin-rebuild

# Tangle all org files to generate configuration files
# Respects local variables in org files (e.g., trailing whitespace cleanup hooks)
.PHONY: tangle
tangle:
	@echo "📝 Tangling org files..."
	emacs --batch \
	  --eval "(setq enable-local-variables :all)" \
	  --eval "(setq enable-local-eval t)" \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(require 'org)" \
	  --eval "(dolist (file '(\"README.org\" \"emacs/README.org\")) \
	    (find-file file) \
	    (message \"📌 Tangling file %s\" file) \
	    (hack-local-variables) \
	    (org-babel-tangle) \
	    (kill-buffer))"
	@echo "✅ Tangling complete"

# Check parity between tangled and committed files
# Fails if org files were modified but tangled outputs weren't committed
# Use this in CI/CD to enforce literate config discipline
# Note: Only checks tangled outputs (.org files are excluded from diff)
.PHONY: check-parity
check-parity:
	@echo "🔄 Checking parity..."
	@if ! git diff --exit-code -- ':!*.org'; then \
		echo "❌ Parity check failed - run 'make tangle' and commit"; \
		exit 1; \
	fi
	@echo "✅ Parity check passed"

# Validate Nix configuration (fast, no actual build)
# Only checks that config evaluates correctly - catches syntax/reference errors
# Use this for quick feedback during development (~5-10 seconds)
.PHONY: validate
validate:
	@echo "🔍 Validating Nix configuration..."
	@nix flake check --no-build
	@nix build .#darwinConfigurations.berlin-4corei7.system --dry-run
	@echo "✅ Validation complete"

# Run all checks locally (simulates CI pipeline)
# Fast checks: tangle, parity, validation (no actual build)
# Use before committing changes
.PHONY: ci
ci: tangle check-parity validate
	@echo "✅ All CI checks passed"

# Build nix-darwin configuration (thorough but slower)
# Actually builds packages - catches compilation errors that validate misses
# Use before merging/deploying (takes minutes depending on cache)
.PHONY: nix-darwin-build
nix-darwin-build:
	${DARWIN_REBUILD} check --flake .#$(shell hostname)

# Deploy nix-darwin configuration (builds and activates)
# Actually switches your system to the new configuration
# Use after testing with nix-darwin-build
.PHONY: nix-darwin-switch
nix-darwin-switch:
	${DARWIN_REBUILD} switch --flake .#$(shell hostname)

# Default target - show help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  make tangle            - Tangle all org files to generate config files"
	@echo "  make check-parity      - Check tangled files match committed files"
	@echo "  make validate          - Validate Nix configuration without building"
	@echo "  make ci                - Run all CI checks locally"
	@echo "  make nix-darwin-build  - Build nix-darwin config"
	@echo "  make nix-darwin-switch - Switch nix-darwin config"

.DEFAULT_GOAL := help
