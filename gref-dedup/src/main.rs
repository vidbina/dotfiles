use std::io::{self, BufRead, BufWriter, Write};
use std::process::Command;

// ANSI color codes matching git's default decoration scheme
const RESET: &str = "\x1b[0m";
const CYAN_BOLD: &str = "\x1b[1;36m";
const GREEN_BOLD: &str = "\x1b[1;32m";
const RED_BOLD: &str = "\x1b[1;31m";
const YELLOW: &str = "\x1b[33m";

/// Strip ANSI escape sequences, returning plain text.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == 0x1b && i + 1 < bytes.len() && bytes[i + 1] == b'[' {
            i += 2;
            while i < bytes.len() && bytes[i] != b'm' {
                i += 1;
            }
            i += 1; // consume 'm'
        } else {
            let c = s[i..].chars().next().unwrap_or('\0');
            out.push(c);
            i += c.len_utf8();
        }
    }
    out
}

/// Return the byte offset in `s` (original, possibly ANSI-coded) corresponding
/// to `text_pos` in the ANSI-stripped version of `s`.
///
/// The returned offset points to either:
/// - an ANSI escape sequence that immediately precedes the target text char, or
/// - the target text char itself if no ANSI code precedes it.
///
/// This means the slice `&s[..offset]` never includes the color code for the
/// target char, and `&s[offset..]` starts with whatever colors/text belongs
/// to that position.
fn text_to_byte(s: &str, text_pos: usize) -> usize {
    let bytes = s.as_bytes();
    let mut tc = 0; // text chars counted so far
    let mut i = 0;
    while i < bytes.len() {
        if tc == text_pos {
            return i;
        }
        if bytes[i] == 0x1b && i + 1 < bytes.len() && bytes[i + 1] == b'[' {
            // skip ANSI sequence
            i += 2;
            while i < bytes.len() && bytes[i] != b'm' {
                i += 1;
            }
            i += 1;
        } else {
            tc += 1;
            i += s[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
        }
    }
    i // text_pos >= len(stripped): return end of string
}

/// Fetch the list of git remote names (one `git remote` call at startup).
/// Returns empty vec on any error (non-git dir, git not on PATH, etc.).
fn get_remotes() -> Vec<String> {
    Command::new("git")
        .args(["remote"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.lines().map(str::to_owned).collect())
        .unwrap_or_default()
}

/// Strip any known remote prefix from a ref name, returning the canonical branch path.
fn canonical<'a>(refname: &'a str, remotes: &[String]) -> &'a str {
    for remote in remotes {
        if let Some(branch) = refname.strip_prefix(&format!("{remote}/")) {
            return branch;
        }
    }
    refname
}

/// Maximum display length for branch names before truncation.
const MAX_BRANCH_LEN: usize = 30;

/// Truncate a branch name to `MAX_BRANCH_LEN`, appending `…` if shortened.
fn truncate_branch(name: &str) -> String {
    if name.chars().count() <= MAX_BRANCH_LEN {
        return name.to_owned();
    }
    let truncated: String = name.chars().take(MAX_BRANCH_LEN - 1).collect();
    format!("{truncated}…")
}

/// Render a new ANSI-colored decoration string from plain inner text.
///
/// `plain_inner` — the decoration text with parens and ANSI stripped, e.g.:
///   `"HEAD -> main, origin/main, origin/vidbina/foo, vidbina/foo, tag: v1.0"`
fn render_decoration(plain_inner: &str, remotes: &[String]) -> String {
    let mut head_branch: Option<&str> = None;
    let mut refs: Vec<&str> = Vec::new();
    let mut tags: Vec<&str> = Vec::new();

    for part in plain_inner.split(", ") {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        if part == "HEAD" {
            // detached HEAD
            head_branch = Some("");
        } else if let Some(b) = part.strip_prefix("HEAD -> ") {
            head_branch = Some(b);
        } else if let Some(t) = part.strip_prefix("tag: ") {
            tags.push(t);
        } else {
            refs.push(part);
        }
    }

    // Check if any remote/HEAD refs exist and collect their remote prefixes.
    // These get folded into the HEAD display rather than shown as separate refs.
    let mut head_remotes: Vec<String> = Vec::new();
    let mut non_head_refs: Vec<&str> = Vec::new();
    for r in &refs {
        let is_remote_head = remotes.iter().any(|rm| *r == format!("{rm}/HEAD"));
        if is_remote_head {
            if let Some(rm) = remotes.iter().find(|rm| r.starts_with(&format!("{rm}/"))) {
                if !head_remotes.contains(rm) {
                    head_remotes.push(rm.clone());
                }
            }
        } else {
            non_head_refs.push(r);
        }
    }

    // Group refs by canonical branch name, preserving first-seen order.
    // Entry: (remote_prefixes_seen, has_local_copy)
    let mut order: Vec<String> = Vec::new();
    let mut groups: std::collections::HashMap<String, (Vec<String>, bool)> =
        std::collections::HashMap::new();

    for r in &non_head_refs {
        let canon = canonical(r, remotes);
        let remote_prefix = remotes.iter().find(|rm| r.starts_with(&format!("{rm}/")));

        let entry = groups.entry(canon.to_owned()).or_insert_with(|| {
            order.push(canon.to_owned());
            (Vec::new(), false)
        });

        if let Some(rm) = remote_prefix {
            if !entry.0.contains(rm) {
                entry.0.push(rm.clone());
            }
        } else {
            entry.1 = true;
        }
    }

    let sep = format!("{YELLOW}, {RESET}");
    let mut parts: Vec<String> = Vec::new();

    // Fold remote prefixes of the HEAD branch into the HEAD display.
    // E.g. HEAD -> foo + origin/foo → [origin/]HEAD -> foo
    let mut head_branch_remotes: Vec<String> = head_remotes.clone();
    if let Some(b) = head_branch {
        if !b.is_empty() {
            let head_canon = canonical(b, remotes).to_owned();
            if let Some((remote_prefixes, _)) = groups.remove(&head_canon) {
                for rm in remote_prefixes {
                    if !head_branch_remotes.contains(&rm) {
                        head_branch_remotes.push(rm);
                    }
                }
                order.retain(|c| *c != head_canon);
            }
        }
    }

    let head_prefix = if !head_branch_remotes.is_empty() {
        let prefix_str = head_branch_remotes.join(",");
        format!("{RED_BOLD}[{prefix_str}/]{RESET}")
    } else {
        String::new()
    };
    match head_branch {
        Some("") => parts.push(format!("{head_prefix}{CYAN_BOLD}HEAD{RESET}")),
        Some(b) => {
            let tb = truncate_branch(b);
            parts.push(format!(
                "{head_prefix}{CYAN_BOLD}HEAD -> {RESET}{GREEN_BOLD}{tb}{RESET}"
            ));
        }
        None => {}
    }

    for canon in &order {
        let (remote_prefixes, has_local) = &groups[canon];
        let tc = truncate_branch(canon);
        if remote_prefixes.is_empty() {
            // local branch only
            parts.push(format!("{GREEN_BOLD}{tc}{RESET}"));
        } else if !has_local && remote_prefixes.len() == 1 {
            // remote-only, single remote — keep full name in red
            parts.push(format!("{RED_BOLD}{}/{tc}{RESET}", remote_prefixes[0]));
        } else {
            // local + remote(s), or multiple remotes — collapse
            let prefix_str = remote_prefixes.join(",");
            parts.push(format!("{RED_BOLD}[{prefix_str}/]{RESET}{GREEN_BOLD}{tc}{RESET}"));
        }
    }

    for tag in &tags {
        parts.push(format!("{YELLOW}tag: {tag}{RESET}"));
    }

    format!(
        "{YELLOW}({RESET}{}{YELLOW}){RESET}",
        parts.join(&sep)
    )
}

/// Check if a single ref token looks like a git ref.
///
/// Matches: `HEAD`, `HEAD -> branch/name`, `tag: v1.0`,
/// `origin/main`, `vidbina/vid-123-slug`, `main`, etc.
///
/// A ref segment is `[a-zA-Z0-9][a-zA-Z0-9_.@-]*`, segments joined by `/`.
fn looks_like_ref(token: &str) -> bool {
    let token = token.trim();
    if token.is_empty() {
        return false;
    }
    if token == "HEAD" || token.starts_with("HEAD -> ") {
        return true;
    }
    if token.starts_with("tag: ") {
        return true;
    }
    token.split('/').all(|seg| {
        !seg.is_empty()
            && seg.chars().next().map(|c| c.is_ascii_alphanumeric()).unwrap_or(false)
            && seg.chars().all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '-' | '@'))
    })
}

/// Check if the content inside parens looks like a git decoration block.
///
/// Requires every comma-separated entry to be a valid ref AND at least one
/// "strong" signal (HEAD, tag:, or a ref containing `/`) to avoid false
/// positives on commit message parens like `(WIP)` or `(see #123)`.
fn looks_like_decoration(inner: &str) -> bool {
    let parts: Vec<&str> = inner.split(", ").collect();
    if parts.is_empty() || !parts.iter().all(|p| looks_like_ref(p)) {
        return false;
    }
    parts.iter().any(|p| {
        let p = p.trim();
        p == "HEAD"
            || p.starts_with("HEAD -> ")
            || p.starts_with("tag: ")
            || p.contains('/')
    })
}

/// Find the byte span of the decoration block `(...)` in `line`.
///
/// Strips ANSI, then scans each `(...)` pair and validates by content — the
/// inner text must look like a comma-separated list of git refs with at least
/// one strong signal. Position-independent: works whether decorations appear
/// before, after, or between other text.
///
/// Returns `None` if no decoration block is found.
fn find_decoration(line: &str) -> Option<(usize, usize)> {
    let stripped = strip_ansi(line);

    // Work with char indices, not byte indices — `text_to_byte` expects char positions.
    // Using byte indices would drift when multi-byte chars (e.g. em dash) precede the decoration.
    let chars: Vec<char> = stripped.chars().collect();
    let mut search_from = 0;
    while search_from < chars.len() {
        let open = match chars[search_from..].iter().position(|&c| c == '(') {
            Some(pos) => search_from + pos,
            None => return None,
        };
        let close = match chars[open..].iter().position(|&c| c == ')') {
            Some(pos) => open + pos,
            None => return None,
        };

        let inner: String = chars[open + 1..close].iter().collect();
        if looks_like_decoration(&inner) {
            return Some((text_to_byte(line, open), text_to_byte(line, close + 1)));
        }

        search_from = open + 1;
    }

    None
}

fn process_line(line: &str, remotes: &[String]) -> String {
    match find_decoration(line) {
        None => line.to_owned(),
        Some((start, end)) => {
            let span = &line[start..end]; // original ANSI-coded span including ( and )
            let plain = strip_ansi(span);
            let inner = plain.trim_matches(|c| c == '(' || c == ')');
            let new_deco = render_decoration(inner, remotes);
            format!("{}{}{}", &line[..start], new_deco, &line[end..])
        }
    }
}

fn main() {
    let remotes = get_remotes();

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());

    for line in stdin.lock().lines() {
        match line {
            Ok(l) => {
                let _ = writeln!(out, "{}", process_line(&l, &remotes));
            }
            Err(_) => break,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_ansi_removes_codes() {
        assert_eq!(strip_ansi("\x1b[33mhello\x1b[0m"), "hello");
        assert_eq!(strip_ansi("no codes"), "no codes");
    }

    #[test]
    fn text_to_byte_plain() {
        let s = "hello";
        assert_eq!(text_to_byte(s, 0), 0);
        assert_eq!(text_to_byte(s, 2), 2);
    }

    #[test]
    fn text_to_byte_with_ansi() {
        // "\x1b[33mAB(CD)\x1b[0m" — '(' is at text pos 2
        let s = "\x1b[33mAB(CD)\x1b[0m";
        let pos = text_to_byte(s, 2);
        assert_eq!(&s[pos..pos + 1], "(");
    }

    #[test]
    fn find_decoration_plain() {
        let line = "* a1b2c3d (HEAD -> main, origin/main) commit msg";
        let span = find_decoration(line).expect("should find decoration");
        let inner = &line[span.0..span.1];
        assert!(inner.contains("HEAD -> main"));
    }

    #[test]
    fn render_dedup_local_and_remote() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("origin/vidbina/foo, vidbina/foo", &remotes);
        // Should contain the deduped form
        assert!(result.contains("[origin/]"));
        assert!(result.contains("vidbina/foo"));
        // Should NOT contain origin/vidbina/foo as a full ref
        assert!(!result.contains("origin/vidbina/foo"));
    }

    #[test]
    fn render_remote_only_kept_full() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("origin/main", &remotes);
        assert!(result.contains("origin/main"));
    }

    #[test]
    fn render_head_target() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("HEAD -> main, origin/main, main", &remotes);
        assert!(result.contains("HEAD -> "));
        assert!(result.contains("[origin/]"));
    }

    #[test]
    fn render_head_with_origin_head() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("HEAD -> vid/topic, origin/HEAD", &remotes);
        assert!(result.contains("[origin/]"));
        assert!(result.contains("HEAD -> "));
        assert!(result.contains("vid/topic"));
        // origin/HEAD should not appear as a separate ref
        assert!(!result.contains("origin/HEAD"));
    }

    #[test]
    fn render_head_full_dedup() {
        let remotes = vec!["origin".to_owned()];
        let result =
            render_decoration("HEAD -> vid/topic, origin/main, origin/HEAD, main", &remotes);
        assert!(result.contains("[origin/]"));
        assert!(result.contains("HEAD -> "));
        assert!(result.contains("vid/topic"));
        // main should be deduped too
        let plain = strip_ansi(&result);
        assert!(!plain.contains("origin/main"));
        assert!(!plain.contains("origin/HEAD"));
    }

    #[test]
    fn render_head_no_origin_head() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("HEAD -> vid/topic", &remotes);
        // No [origin/] prefix when origin/HEAD is absent
        let plain = strip_ansi(&result);
        assert!(!plain.contains("[origin/]HEAD"));
        assert!(plain.contains("HEAD -> vid/topic"));
    }

    #[test]
    fn render_detached_head_with_origin() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration("HEAD, origin/HEAD, origin/main", &remotes);
        let plain = strip_ansi(&result);
        assert!(plain.contains("[origin/]HEAD"));
        assert!(!plain.contains("origin/HEAD"));
    }

    #[test]
    fn render_head_branch_with_remote_counterpart() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration(
            "HEAD -> vidbina/foo, origin/vidbina/foo",
            &remotes,
        );
        let plain = strip_ansi(&result);
        // Should collapse to [origin/]HEAD -> vidbina/foo
        assert!(plain.contains("[origin/]HEAD -> vidbina/foo"));
        // origin/vidbina/foo should NOT appear as a separate ref
        assert!(!plain.contains(", origin/vidbina/foo"));
    }

    #[test]
    fn render_head_branch_remote_and_other_refs() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration(
            "HEAD -> vidbina/foo, origin/vidbina/foo, origin/main, main",
            &remotes,
        );
        let plain = strip_ansi(&result);
        assert!(plain.contains("[origin/]HEAD -> vidbina/foo"));
        assert!(plain.contains("[origin/]main"));
        assert!(!plain.contains(", origin/vidbina/foo"));
    }

    #[test]
    fn truncate_short_name_unchanged() {
        assert_eq!(truncate_branch("main"), "main");
        assert_eq!(truncate_branch("vidbina/dev"), "vidbina/dev");
    }

    #[test]
    fn truncate_long_name() {
        let long = "vidbina/vid-655-fix-glog-move-from-zsh-function-to-git-alias";
        let result = truncate_branch(long);
        assert!(result.ends_with('…'));
        assert_eq!(result.chars().count(), 30);
    }

    #[test]
    fn render_truncates_long_branch() {
        let remotes = vec!["origin".to_owned()];
        let result = render_decoration(
            "origin/vidbina/vid-655-fix-glog-move-from-zsh-function-to-git-alias, vidbina/vid-655-fix-glog-move-from-zsh-function-to-git-alias",
            &remotes,
        );
        let plain = strip_ansi(&result);
        assert!(plain.contains('…'));
        assert!(!plain.contains("to-git-alias"));
    }

    #[test]
    fn no_decoration_passes_through() {
        let line = "| graph line only";
        assert_eq!(process_line(line, &[]), line);
    }

    #[test]
    fn subject_parens_ignored() {
        // Content doesn't look like refs — no strong signal
        let line = "* a1b2c3d fix: update (closes #123)";
        assert_eq!(process_line(line, &[]), line);
    }

    #[test]
    fn wip_parens_ignored() {
        let line = "* a1b2c3d fix (WIP) some message";
        assert_eq!(process_line(line, &[]), line);
    }

    #[test]
    fn decoration_at_end_of_line() {
        let remotes = vec!["origin".to_owned()];
        let line = "* a1b2c3d commit message (HEAD -> main, origin/main, main)";
        let result = process_line(line, &remotes);
        assert!(result.contains("[origin/]"));
        assert!(result.starts_with("* a1b2c3d commit message"));
    }

    #[test]
    fn decoration_at_end_remote_only() {
        let remotes = vec!["origin".to_owned()];
        let line = "* a1b2c3d commit message (HEAD -> feature, origin/main)";
        let result = process_line(line, &remotes);
        let plain = strip_ansi(&result);
        assert!(plain.contains("origin/main"));
        assert!(plain.contains("HEAD -> feature"));
    }

    #[test]
    fn decoration_after_non_ref_parens() {
        let remotes = vec!["origin".to_owned()];
        let line = "* a1b2c3d fix (WIP) (HEAD -> main, origin/main, main) more text";
        let result = process_line(line, &remotes);
        assert!(result.contains("(WIP)"));
        assert!(result.contains("[origin/]"));
    }

    #[test]
    fn multibyte_subject_with_parens_and_decoration() {
        // Reproduces VID-682: em dash (3-byte UTF-8) + subject parens + long branch
        // decoration. The `(decisions)` parens should be skipped (not a decoration),
        // and the actual decoration should be cleanly replaced without garbling HEAD.
        let remotes = vec!["origin".to_owned()];
        let line = "* 0b2cb36 docs(decisions): lock in disclosure schema \u{2014} update-in-place, scope boundary [ai:claude] (HEAD -> vidbina/idt-11-live-congressional-disclosures-flowing-into-the-system)";
        let result = process_line(line, &remotes);
        let plain = strip_ansi(&result);
        // The subject must be preserved intact (including em dash and parens)
        assert!(plain.contains("docs(decisions)"));
        assert!(plain.contains("\u{2014}"));
        // The decoration must start with `(HEAD` — not `H(EAD` or other garbling
        assert!(plain.contains("(HEAD -> "));
        // The branch name should be truncated (it's > 30 chars)
        assert!(plain.contains("\u{2026}"));  // ellipsis from truncation
        // No double parens or leaked characters
        assert!(!plain.contains("H(EAD"));
        assert!(!plain.contains("(("));
    }

    #[test]
    fn multibyte_subject_with_ansi_and_decoration() {
        // Same scenario but with ANSI color codes as git would produce with %C(auto)
        let remotes = vec!["origin".to_owned()];
        let line = "* \x1b[33m0b2cb36\x1b[0m docs(decisions): lock in disclosure schema \u{2014} update-in-place, scope boundary [ai:claude]\x1b[33m (\x1b[1;36mHEAD -> \x1b[1;32mvidbina/idt-11-live-congressional-disclosures-flowing-into-the-system\x1b[33m)\x1b[0m";
        let result = process_line(line, &remotes);
        let plain = strip_ansi(&result);
        assert!(plain.contains("docs(decisions)"));
        assert!(plain.contains("\u{2014}"));
        assert!(plain.contains("(HEAD -> "));
        assert!(!plain.contains("H(EAD"));
        assert!(!plain.contains("(("));
    }

    #[test]
    fn decoration_requires_strong_signal() {
        assert!(!looks_like_decoration("WIP"));
        assert!(!looks_like_decoration("FIXME"));
        assert!(looks_like_decoration("HEAD -> main"));
        assert!(looks_like_decoration("origin/main"));
        assert!(looks_like_decoration("tag: v1.0"));
    }
}
