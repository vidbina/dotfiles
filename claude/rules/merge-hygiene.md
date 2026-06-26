Before merging any PR, verify its base branch is correct — never assume. For stacked PRs, check that child PRs have been retargeted before deleting the merged branch.

Use `/pr` for all merges. It reads the repo's merge convention, handles stack ordering, and checks for child PRs automatically. Do not merge from the GitHub web UI for stacked PRs — it doesn't enforce retargeting.
