When Touch ID is unavailable (remote-control sessions, walk-away agents, SSH sessions), use these git aliases instead of the standard commands:

- `git mcommit` instead of `git commit` — skips SSH signing. Commits will show as "Unverified" on GitHub, which is the intended signal for AI-produced work.
- `git mpush` instead of `git push` — routes through HTTPS via `gh` auth token instead of SSH/1Password.
- `git mpull` instead of `git pull` — fetches through HTTPS via `gh` auth token instead of SSH/1Password.

All aliases are defined in the dotfiles nix config and available on all machines managed by this setup.

When working locally with Touch ID available, use the normal `git commit`, `git push`, and `git pull` — signed commits are preferred for human-reviewed work.
