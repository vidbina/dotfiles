Run one simple shell command per tool call. Don't chain with `&&`, `;` or `|`, don't prefix with `cd dir &&`, and don't use `$( … )` unless there is no other way.

Why: a chain is hard to judge at an approval prompt, especially on a phone. It also misses per-command allowlists such as `Bash(git status *)`, so the person gets prompted anyway.

## Instead of a chain

- **Read, search or list files with the dedicated tools** (Read, Grep, Glob), not `cat | grep | head`. They page their own output.
- **Shape output with the command's own flags**, not a pipe into `head`, `grep`, `sed` or `jq`: `gh … --json … -q …`, `git log -5 --oneline`, `grep -n -A 5`.
- **Point at a directory with a flag**, not `cd`: `git -C <dir>`, `make -C <dir>`. If a tool really must run from inside a directory, say why.
- **Write long or multi-line text to a file** with the Write tool and pass it by flag (`--body-file`, `-F`, `< input.txt`), not a heredoc or a long quoted argument.
- **Independent commands go in separate calls**, sent in parallel if nothing depends on them. A command that depends on another's output waits for that result.
- **Don't wait in a loop.** No `sleep` or `until` loops to poll CI or a service. Check once, report what you see, and leave the watching to whoever owns it.
