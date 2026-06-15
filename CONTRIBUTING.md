# Contributing

## Literate config and tangled files

This repo uses a literate config approach. `README.org` is the canonical source for most nix configuration files. Org-babel tangles source blocks from `README.org` into their target files using noweb references.

### Identifying tangled files

Tangled files carry a comment on **line 1** indicating their source:

```
# Tangled from README.org
```

This is a repo convention. If a file has this comment, its canonical source lives in `README.org`.

### Making changes

For straightforward changes — adding a package, tweaking a setting — start directly in `README.org`. Edit the source block, `make tangle`, verify, done. This is the recommended default.

For more complex work, it often makes sense to work outside-in: build the thing first, wire it in later. For example, when adding a new Rust tool like `gref-dedup`, the natural flow was:

1. Write the Rust crate in its own directory, get it compiling and working.
2. Only then figure out where in the nix config to reference it (which `noweb-ref`, which package list).
3. Only then add the source block to `README.org` with documentation explaining what it does.

Editing tangled output files directly is also fine for rapid iteration — test a change, see if it builds, fix it, repeat. This avoids the overhead of round-tripping through `README.org` on every attempt when you're still figuring out what works.

However you get there, the finishing steps are the same:

1. Ensure the change is in the corresponding source block in `README.org` (search for the `noweb-ref` name or the target filename), with documentation around the code block.
2. Run `make tangle` to regenerate all tangled output files.
3. Run `make verify-parity` to confirm that the tangled outputs match `README.org`.
4. Commit both `README.org` and the regenerated output files together.

The key rule: **parity must hold at commit time.** How you get there is up to you.

### Parity checks

Always run `make verify-parity` before committing changes that touch `README.org` or any tangled file. This target diffs the tangled output against the working tree — any mismatch means either:

- You edited a tangled file but haven't ported the change back to `README.org` yet, or
- You edited `README.org` but forgot to re-tangle (fix: run `make tangle`).

CI runs the same check, so commits with parity drift will fail.

## AI Co-authorship

Use both trailers on every AI-assisted commit. `Co-authored-by` works with GitHub today; `Assisted-by` is the emerging standard but forges don't render it yet. Using both ensures nothing is lost as tooling catches up:

```
Co-authored-by: Claude Code <noreply@anthropic.com>
Assisted-by: Claude:claude-opus-4-6
```

**`Co-authored-by:`** is the established Git/GitHub convention. GitHub renders it as a secondary author avatar on the commit. The email is a provenance marker, not a contact address — it follows GitHub's `noreply@` convention. Use `noreply@{provider-domain}` (e.g. `noreply@anthropic.com`, `noreply@openai.com`, `noreply@google.com`).

**`Assisted-by:`** is the emerging standard from the [Linux kernel guidelines](https://github.com/torvalds/linux/blob/master/Documentation/process/coding-assistants.rst#attribution). Format: `AGENT_NAME:MODEL_VERSION`. No email field — sidesteps the fake-email problem. Not yet widely supported by forges, but forward-compatible.

Once forges render `Assisted-by` natively, `Co-authored-by` can be dropped.

Both trailers survive squash-merge in the commit body, so provenance is preserved even when individual commits are collapsed.

## PR Title Convention

PR titles follow the same conventional-commit format as individual commits, with a ticket ID suffix for traceability:

```
<type>(<scope>): <subject> [TICKET-ID]
```

- **type** and **scope** — same rules as commits (see `AGENTS.md` commit conventions)
- **subject** — imperative mood, value-oriented, distinguishing phrase first
- **`[TICKET-ID]`** — the Linear ticket ID in square brackets, e.g. `[VID-31]`. Auto-injected from the branch name by the `/pr` skill. Omit only for ad-hoc branches without a ticket.

### Examples

```
feat(auth): enable Google login [VID-31]
fix: handle null response from upstream [VID-45]
doc: update contributing guide with PR title convention [VID-687]
chore(ci): enforce semantic PR titles [VID-687]
```

### Why this format?

Individual commits and merged PRs serve different audiences in `git log`:

| Layer | Format | Ticket ID? | When visible |
|---|---|---|---|
| Individual commit | `type(scope): subject [ai:agent]` | No — branch encodes it | Always |
| Merged PR (squash) | `type(scope): subject [TICKET-ID] (#N)` | Yes — for traceability | After squash-merge |
| Merged PR (merge commit) | `type(scope): subject [TICKET-ID] (#N)` | Yes — on merge commit | After merge-commit |

The ticket ID suffix is the visual signal that distinguishes a squash-merged PR from an individual commit when scanning `git log`. It also makes every merged PR traceable back to its Linear ticket without opening GitHub.

### CI enforcement

The `amannn/action-semantic-pull-request` GitHub Action validates PR titles on open/edit. See `.github/workflows/lint-pr.yaml`.
