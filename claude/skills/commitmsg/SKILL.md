---
name: commitmsg
description: "Use this skill when the user asks for a commit message suggestion based on their current uncommitted changes — or when the user invokes `/commitmsg`. Trigger for prompts like 'suggest a commit message', 'commitmsg', 'what should I commit this as', 'draft a commit', 'write a commit message for this'. The skill reads the repo's own docs (CONTRIBUTING.md, AGENTS.md, README.md) and the org knowledge base (via `./kb` or `./knowledge-base` symlink if present) for commit conventions, inspects the uncommitted diff and recent git history, and prints a suggested commit message in chat. It never commits, stages, or modifies files in normal mode. It also has a setup mode (`--setup`) that inspects git history, repo docs, and the org kb to draft or update the commit convention section in the repo's docs — run this once per repo to bootstrap the convention. Do NOT trigger for actually committing (the user runs git commit themselves), for amending or rebasing (those are direct git operations), for generating changelogs or release notes (different task), or for reviewing code (use a review skill or do it directly)."
allowed-tools: Bash Glob Grep Read Write Edit AskUserQuestion
---

# commitmsg

Suggest a **commit message** for the user's current uncommitted changes, following the conventions documented in the repo itself.

The defining design principles of this skill are:

> **The repo's own docs are the source of truth for commit conventions.** The skill reads CONTRIBUTING.md, AGENTS.md, README.md — wherever the repo documents its convention. It does not hardcode any format.
>
> **Output only.** In normal mode, the skill prints a suggested commit message in chat and nothing else. No staging, no committing, no file writes. Git is HITL.
>
> **Setup bootstraps the convention into the repo's docs.** The `--setup` mode inspects git history, existing docs, and the org kb to draft the commit convention section. Run once per repo, then normal mode reads what setup wrote.

## Normal mode (default)

### Step 1 — Find the convention

Search for commit convention documentation. Check these sources in order, collecting guidance from all that exist (later sources add context, they don't override earlier ones):

1. **Repo docs (primary):**
   - `CONTRIBUTING.md` — a "Commit" or "Commit Messages" or "Commit Strategy" section
   - `CLAUDE.md` / `AGENTS.md` — a "Commit Strategy" or "Commit" section
   - `README.md` — a "Commit Standards" or "Contributing" section that mentions commits
   - `.github/CONTRIBUTING.md` or `docs/CONTRIBUTING.md`

2. **Org knowledge base (supplementary):** Check for a `./kb` or `./knowledge-base` symlink at the repo root. This is the org convention for providing AI tooling access to the shared engineering handbook. If present, read:
   - `{kb-symlink}/templates/AGENTS.md` — the "Commit Strategy" section
   - `{kb-symlink}/templates/README.md` — the "Commit Standards" section

   Where `{kb-symlink}` is whichever of `./kb` or `./knowledge-base` exists (check `./kb` first).

   If neither symlink exists, that's fine — the repo's own docs are sufficient. Setting up the kb symlink is a human operation; the skill never creates it.

3. **Git history (implicit convention):** `git log --oneline -20` — if the docs are sparse, the recent commit history itself reveals the de facto convention (prefixes in use, tag patterns, message length norms).

If none of these sources contain commit convention guidance, warn the user:

> No commit convention found in this repo's docs. Run `/commitmsg --setup` to bootstrap one from git history and the org kb, or I'll fall back to describing the changes without a specific format.

Proceed with a plain descriptive message if the user doesn't want to set up.

### Step 2 — Read the diff

Run via `Bash`:

- `git status` — to see what's staged vs unstaged, untracked files
- `git diff --cached` — staged changes (this is what `git commit` will include)
- `git diff` — unstaged changes (flag these as not-yet-staged)
- `git log --oneline -10` — recent commits for style continuity

If nothing is staged, note this and base the suggestion on the full uncommitted diff (`git diff` + untracked). Mention that the user needs to stage before committing.

### Step 3 — Draft the message

Analyze the changes:

- **What changed:** files touched, nature of the changes (new feature, bug fix, refactor, docs, chore, test, etc.)
- **Why it changed:** infer from the diff context, variable names, comments, test names. If the "why" isn't clear from the diff alone, say so rather than fabricating intent.
- **Scope:** if the convention uses scopes (e.g., `feat(auth):`), derive from the primary directory or module affected.

Apply the convention found in Step 1 to format the message. If the convention specifies:
- A prefix format (e.g., `feat:`, `fix:`) — use it
- A tag (e.g., `[ai:claude]`) — include it
- A co-author line — include it
- A max length — respect it
- A body/footer structure — follow it

Print the suggested message in a fenced code block so the user can copy it.

If the changes span multiple unrelated concerns, suggest breaking into multiple commits and provide a message for each.

### Step 4 — Done

No further action. The user copies the message and runs `git commit` themselves.

## Setup mode (`--setup`)

Triggered when the user passes `--setup` or says something like "set up commit conventions for this repo."

### Step 1 — Inspect existing state

In parallel:

- **Git history:** `git log --oneline -50` — look for patterns: conventional commits, prefixes, tags, co-author lines, scope usage, message length
- **Repo docs:** read CONTRIBUTING.md, CLAUDE.md, AGENTS.md, README.md for any existing commit guidance
- **Org kb:** check for `./kb` or `./knowledge-base` symlink. If present, read `templates/AGENTS.md` and `templates/README.md` for the org-level commit convention. If absent, check whether one should be set up (the org convention is to symlink `./kb` or `./knowledge-base` → the shared kb repo) and mention it to the user.

Synthesize: what convention is the repo already following (even if undocumented)? Does it match the org kb template? Are there inconsistencies?

### Step 2 — Draft the convention

Print the proposed convention section in chat. This should cover:

- **Format:** the commit message structure (prefix, scope, subject, body, footer)
- **Prefixes:** which ones are used and what they mean
- **Tags:** any AI attribution tags
- **Co-authorship:** how to attribute AI-assisted commits
- **Length limits:** subject line max, body wrapping
- **Multi-concern rule:** one commit per logical change

Base this on what the git history actually shows, reconciled with the org kb if available. If the org kb has a convention and the repo doesn't, propose adopting the org convention. If the repo has a convention that diverges from the org kb, flag the divergence for the user to resolve.

### Step 3 — Confirm and write

Use `AskUserQuestion` to confirm:

- **Where to write:** CONTRIBUTING.md (create or append section) / AGENTS.md (append section) / README.md (append section) / Other
- **Content looks good?** with the drafted section shown in chat

On confirmation, write or append the section to the chosen file via `Write` or `Edit`.

### Step 4 — Done

Print the path written and suggest a commit message for the convention documentation itself.

## Anti-patterns

- **Don't hardcode a commit format.** The convention lives in the repo's docs. If there's no convention, say so — don't silently impose one.
- **Don't commit or stage.** The skill outputs text. Git operations are HITL.
- **Don't modify files in normal mode.** Only `--setup` writes to the repo.
- **Don't fabricate intent.** If the "why" behind a change isn't clear from the diff, say "the intent isn't clear from the diff — you may want to add context in the commit body" rather than guessing.
- **Don't suggest amending previous commits.** That's a destructive operation the user decides on their own.
- **Don't hardcode the kb path.** The org kb is discovered via `./kb` or `./knowledge-base` symlink, not via an absolute path. If the symlink isn't there, the kb isn't available — don't go hunting.
