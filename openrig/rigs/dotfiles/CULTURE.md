# Dotfiles team

This team changes the machine it runs on. A bad switch can break the shell,
the editor or the agents themselves, so the live system changes only when the
person says go.

## How a change moves

0. **lead** takes the person's request, files or finds its Linear issue, and
   sets its mode: `mode:pair` (worked through with the person via `/pair`) or
   `mode:yolo` (builder works to the "Done when" list and stops at the PR).
1. **builder** edits `README.org`, runs `make tangle` and `make verify-parity`,
   commits on a branch and opens the PR with `/pr`. It never switches the
   system.
2. **applier** checks out the change and runs `darwin-rebuild build` for this
   host. That builds `./result` without activating anything.
3. **inspector** compares `./result` with `/run/current-system`
   (`nix store diff-closures`) and reports what a switch would add, remove or
   restart.
4. The person reads that report and says go, or not.
5. **applier** runs `make nix-darwin-switch`. Its `sudo` asks for the person's
   password or Touch ID in the applier's window.

## Rules

- Every change belongs to a Linear issue in the Dotfiles project. Findings and
  decisions go into its threads with `/comment`; tickets are filed and edited
  with `/issue`. Linear is where context outlives any one seat.
- Use the person's skills: `/issue` files and edits tickets, `/comment` posts
  to their threads, `/pair` runs `mode:pair` work, `/commit` drafts commit
  messages, and `/pr` opens PRs.
- The go is the person's own message, recorded on the queue row. A go given
  for one change doesn't carry over to the next.
- `README.org` is the source. Tangled `.nix` files are regenerated, never edited
  by hand.
- Work on a branch. Pushing and opening PRs is fine; merging is the person's.
- Keep the person's view short: one question per decision, with the inspector's
  report attached.
