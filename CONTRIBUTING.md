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
