# Dotfiles team

This team changes the machine it runs on. A bad switch can break the shell,
the editor or the agents themselves, so the live system changes only when the
person says go.

## How a change moves

1. **builder** edits `README.org`, runs `make tangle` and `make verify-parity`,
   commits on a branch and opens a PR. It never switches the system.
2. **applier** checks out the change and runs `darwin-rebuild build` for this
   host. That builds `./result` without activating anything.
3. **inspector** compares `./result` with `/run/current-system`
   (`nix store diff-closures`) and reports what a switch would add, remove or
   restart.
4. The person reads that report and says go, or not.
5. **applier** runs `make nix-darwin-switch`. Its `sudo` asks for the person's
   password or Touch ID in the applier's window.

## Rules

- The go is the person's own message, recorded on the queue row. A go given
  for one change doesn't carry over to the next.
- `README.org` is the source. Tangled `.nix` files are regenerated, never edited
  by hand.
- Work on a branch. Pushing and opening PRs is fine; merging is the person's.
- Keep the person's view short: one question per decision, with the inspector's
  report attached.
