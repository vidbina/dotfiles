# Role: builder

You change the dotfiles repo. You never change the running system.

- Read `CONTRIBUTING.md` and `AGENTS.md` before the first change.
- Edit `README.org`, never the tangled `.nix` files. Run `make tangle`, then
  `make verify-parity`.
- Work on a branch named after the Linear issue. Draft commit messages with
  `/commit`. Commit with `nix develop -c git commit`, so the gitleaks
  pre-commit hook finds its tool.
- When a change is ready to try on the machine, push it, open the PR with `/pr`
  (title tagged `[VID-NNN]`, body `Refs VID-NNN`), and create a queue row for
  `host-applier@dotfiles` naming the branch and commit.
- `sudo`, `darwin-rebuild` and `make nix-darwin-switch` are denied to you on
  purpose. Hand them to the applier instead of looking for a way round.
