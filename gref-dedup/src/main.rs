use std::io::{self, BufRead, BufWriter, Write};
use std::process::Command;

// ANSI color codes matching git's default decoration scheme
const RESET: &str = "\x1b[0m";
const DIM: &str = "\x1b[2m";
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

    // HEAD pointer first, with remote prefix if origin/HEAD was present
    let head_prefix = if !head_remotes.is_empty() {
        let prefix_str = head_remotes.join(",");
        format!("{DIM}[{prefix_str}/]{RESET}")
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
            parts.push(format!("{DIM}[{prefix_str}/]{RESET}{GREEN_BOLD}{tc}{RESET}"));
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

/// Find the byte span of the decoration block `(...)` in `line`.
///
/// Strips ANSI to locate the `(` and `)` in plain text, validates that the
/// token immediately before `(` looks like a git short hash (7–40 hex chars),
/// then maps the positions back to byte offsets in the original ANSI string.
///
/// Returns `None` if no decoration block is found or the heuristic doesn't match.
fn find_decoration(line: &str) -> Option<(usize, usize)> {
    let stripped = strip_ansi(line);

    let open = stripped.find('(')?;

    // Validate: last whitespace-delimited token before '(' is a hex short hash
    let before = stripped[..open].trim_end();
    let last_tok = before.split_whitespace().last()?;
    if last_tok.len() < 7
        || last_tok.len() > 40
        || !last_tok.chars().all(|c| c.is_ascii_hexdigit())
    {
        return None;
    }

    let close = stripped[open..].find(')')? + open;

    Some((text_to_byte(line, open), text_to_byte(line, close + 1)))
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
        // Subject contains parens but no decoration block after hash
        let line = "* a1b2c3d fix: update (closes #123)";
        // The ( appears after non-hex token "fix:", so no decoration found
        assert_eq!(process_line(line, &[]), line);
    }
}
