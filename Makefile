SHELL = /usr/bin/env sh

HOME_MANAGER = home-manager
NIX_SHELL = nix-shell
DARWIN_REBUILD = sudo darwin-rebuild
HOSTNAME := $(shell hostname)

# Third-party Homebrew taps that must be trusted before `brew bundle` runs —
# Homebrew refuses to load formulae/casks from untrusted taps. `brew trust` is
# idempotent, so we just re-assert these on every switch (no state to track).
# Add new third-party taps here.
TRUSTED_TAPS = felixkratz/formulae anomalyco/tap anthropics/tap

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

# Verify parity between tangled outputs and committed files.
# Fails if org sources were modified but tangled outputs weren't committed.
# Note: Only checks tangled outputs (.org files are excluded from diff)
.PHONY: verify-parity
verify-parity:
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
	@nix build .#darwinConfigurations.berlin26-m5pro.system --dry-run
	@echo "✅ Validation complete"

# Verify literate config integrity: tangle org sources, confirm parity
# with committed outputs, and validate the resulting Nix configuration.
# Run before pushing. Fast path (~10s with warm cache): no build step.
.PHONY: check-config
check-config: tangle verify-parity validate
	@echo "✅ All checks passed"

# Build nix-darwin configuration (thorough but slower)
# Actually builds packages - catches compilation errors that validate misses
# Use before merging/deploying (takes minutes depending on cache)
.PHONY: nix-darwin-build
nix-darwin-build:
	${DARWIN_REBUILD} check --flake .#${HOSTNAME}

# Trust third-party Homebrew taps so `brew bundle` (run during the switch)
# doesn't abort on an untrusted tap. Idempotent; runs as the invoking user,
# which is the context whose trust.json the bundle reads.
.PHONY: trust-taps
trust-taps:
	@echo "🔐 Trusting third-party Homebrew taps..."
	@for tap in ${TRUSTED_TAPS}; do brew trust "$$tap"; done

# Deploy nix-darwin configuration (builds and activates)
# Actually switches your system to the new configuration
# Use after testing with nix-darwin-build
.PHONY: nix-darwin-switch
nix-darwin-switch: trust-taps
	${DARWIN_REBUILD} switch --flake .#${HOSTNAME}

# Report drift between the declared model set and what ollama actually has.
# Read-only — it never pulls or removes anything. Deliberately NOT part of
# check-config: it reflects machine state and needs a running ollama, whereas
# check-config validates the repo and runs in CI where neither exists.
.PHONY: ollama-sync
ollama-sync:
	@echo "🔍 Reconciling ollama models against ollama-models.txt..."
	@tmp=$$(mktemp -d); \
	grep -vE '^[[:space:]]*(#|$$)' ollama-models.txt | awk 'NF {print $$1}' | sort -u > $$tmp/declared; \
	ollama list | tail -n +2 | awk 'NF {print $$1}' | sort -u > $$tmp/installed; \
	missing=$$(comm -23 $$tmp/declared $$tmp/installed); \
	extra=$$(comm -13 $$tmp/declared $$tmp/installed); \
	rm -rf $$tmp; \
	if [ -n "$$missing" ]; then \
	  echo "❌ Declared but not installed — run 'make ollama-pull':"; \
	  echo "$$missing" | sed 's/^/     /'; \
	fi; \
	if [ -n "$$extra" ]; then \
	  echo "⚠️  Installed but not declared — adopt in README.org or 'ollama rm':"; \
	  echo "$$extra" | sed 's/^/     /'; \
	fi; \
	if [ -z "$$missing" ] && [ -z "$$extra" ]; then \
	  echo "✅ Model set matches the manifest"; \
	fi

# Pull every declared model. `ollama pull` is idempotent — it fetches what is
# missing and updates what is stale — so this one operation covers both cases
# without needing to introspect which is which. That is why ollama-sync does
# not try to detect staleness: re-running the idempotent op is cheaper and
# more reliable than querying the registry per model.
.PHONY: ollama-pull
ollama-pull:
	@grep -vE '^[[:space:]]*(#|$$)' ollama-models.txt | awk 'NF {print $$1}' | while read -r model; do \
	  echo "⬇️  $$model"; \
	  ollama pull "$$model"; \
	done
	@echo "✅ All declared models present and current"

# Default target - show help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  make tangle            - Tangle all org files to generate config files"
	@echo "  make verify-parity     - Verify tangled outputs match committed files"
	@echo "  make validate          - Validate Nix configuration without building"
	@echo "  make check-config      - Verify literate config (tangle + parity + nix validate)"
	@echo "  make nix-darwin-build  - Build nix-darwin config"
	@echo "  make nix-darwin-switch - Switch nix-darwin config"
	@echo "  make ollama-sync       - Report drift between ollama-models.txt and ollama list"
	@echo "  make ollama-pull       - Pull/update every declared ollama model (idempotent)"

.DEFAULT_GOAL := help
