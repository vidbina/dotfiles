Every PR and every key capability change starts from a Linear issue. Plan in the issue before you change anything.

Why: the person should not have to ask for a plan each time. The issue is where context outlives any one session, seat or machine.

## The flow

1. **`/issue` first.** Before work starts, find the issue that covers it or file one with `/issue`. Check current branches and related issues before filing, so you don't create a duplicate.
2. **`/comment` as you go.** Findings, decisions and changes of plan go into the issue's numbered threads with `/comment`, when they happen. Don't save them for a summary at the end. Keep the issue body as the original intent.
3. **`/pair` is the main way work happens.** Commits and comments flow through `/pair` (`mode:pair`). In yolo mode the agent works to the issue's "Done when" list on its own and stops at the PR. The PR is the gate.
4. **`/pr` opens the PR.** Tag the title with the issue ID (`[VID-123]`) and reference the issue in the body.

## What counts as a key capability change

Anything a person would notice or need to reverse: new or removed tools, services, permissions, agent seats or rules, and changes to how work gets approved or applied. This includes changes outside any repo, such as local rig specs and OpenRig operations. File an issue for those too, even though no PR follows.

Typos, small edits in a PR that already has an issue, and read-only investigation don't need their own issue.

## Repos without a Linear project

Some repos have no Linear project. The blog is one. Track their work in the repo's `docs/TODO.md` and tag the PR title `[noticket]`. `/pr` still opens the PR.
