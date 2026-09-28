Before merging any PR, verify its base branch is correct — never assume. For stacked PRs, check that child PRs have been retargeted before deleting the merged branch.

Use `/pr` for all merges. It reads the repo's merge convention, handles stack ordering, and checks for child PRs automatically. Do not merge from the GitHub web UI for stacked PRs — it doesn't enforce retargeting.

## Never call a PR ready without checking its checks first

"Ready to merge", "good to go", "all green" — none of these may be said from memory, from a local test run, or from a check result that predates the current head. Read the checks, then speak.

**A push invalidates every previous result.** A green run belongs to the commit it ran on. After pushing, the honest status is *unknown until CI reports* — say that, or wait and check. Do not carry a previous green forward across a push, and do not infer one from local tests passing: the checks that catch generated-artifact drift, cross-machine formatting and config differences are exactly the ones that cannot fail locally.

**Read the checks with `statusCheckRollup`**, not `gh pr checks` — a required context with no producing job never reports at all and is invisible to the latter:

```
gh pr view <N> --json statusCheckRollup -q '.statusCheckRollup[] | "\(.name // .context): \(.conclusion // .state)"'
gh api repos/<org>/<repo>/branches/<base>/protection --jq '.required_status_checks.contexts[]'
```

**Report the state that exists, including the parts that are not green.** If a check is pending, failing, or permanently stuck, name it and say why it does not block — do not omit it because it is non-required. `mergeStateStatus: UNSTABLE` means something is not green; say which thing.

**A failing check is work, not a notification.** Diagnose and fix it in the same turn rather than reporting it back. The person asking is usually on a phone and cannot act on it.

**When a failure is in a generated artifact, the cause is upstream of it.** A freshness check fails on the artifact but the bug is in the source it is generated from. Reproduce the check locally — regenerate, then `git diff --exit-code` — instead of regenerating until the diff disappears.

## Never run a formatter the repo does not configure

Before invoking any formatter, confirm the repo configures it (`.prettierrc`, `.editorconfig`, a `format` script, a linter that owns style). With no config, the tool applies its own defaults and rewrites the whole file to a style the repo does not use — hundreds of lines of churn around a small change, and a diff nobody reads. Use the repo's own command (`npm run format`, `ruff format`) or leave formatting alone and match the surrounding style by hand.
