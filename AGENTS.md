# Agent Usage Guidelines

## 🔥 CRITICAL RULE: ALWAYS WRITE COMMAND OUTPUT TO FILES

### ⚠️ MANDATORY for ALL commands with potentially long output:
- **NEVER** run commands directly that might produce more than a few lines
- **ALWAYS** redirect output to files: `command > output.txt 2>&1`
- **ALWAYS** use files for post-exec grepping and re-analysis

### ❌ DO NOT Use Agent Tool For:
- Long search operations that might produce extensive output
- Complex analysis that could generate large results
- Any operation where the output might be truncated or overwhelming

### ✅ Instead, Use Direct Tools:
- **Bash tool**: Write command output to files for later analysis
- **Grep tool**: For targeted searches
- **Read tool**: For examining specific files
- **Glob tool**: For finding files by pattern

## 🔥 CRITICAL RULE: ALWAYS PREFER DECLARATIVE SYSTEM CHANGES

This repo uses literate config — `README.org` is the system config. Editing it IS the system change, expressed declaratively and version-controlled. The user runs `darwin-rebuild switch` (or `make switch`) to apply, as part of their normal workflow.

### ⚠️ MANDATORY for ALL package/service/config changes:
- **NEVER** run or suggest imperative installs (`brew install`, `gh extension install`, `pip install`, `npm install -g`, `cargo install`, etc.) — these bypass version control and leave ghost state
- **ALWAYS** make the change in `README.org` (via the appropriate `noweb-ref` block) or the relevant nix module — that IS the install

### Declarative paths in this repo:
- **Homebrew brews:** `#+begin_src nix :noweb-ref homebrew-brews` in `README.org`
- **Homebrew casks:** `#+begin_src nix :noweb-ref homebrew-casks` in `README.org`
- **Nix packages:** `#+begin_src nix :noweb-ref dev-packages` or system/home-manager modules
- **Services:** use nix-darwin launchd via object-form brew entry (`start_service = true`) or home-manager service modules

### ❌ Never do imperatively:
- `brew install <pkg>` → add to `homebrew-brews` noweb-ref instead
- `brew install --cask <pkg>` → add to `homebrew-casks` noweb-ref instead
- `gh extension install <ext>` → check if a standalone cask or nix package exists; prefer that
- `pip install`, `npm install -g`, `cargo install` → use nix packages or home-manager

## Best Practices

### For Long Command Output:
```bash
# Instead of running commands that produce long output directly
command_with_long_output > analysis_results.txt 2>&1

# Then read the file
# Use Read tool to examine analysis_results.txt
```

### For Comprehensive Searches:
```bash
# Write search results to files
grep -r "pattern" . > search_results.txt 2>&1
find . -name "*.nix" -exec grep -l "problematic_package" {} \; > problematic_files.txt
```

### For Build Traces:
```bash
# Capture build traces to files for analysis
nix build .#config --show-trace > build_trace.txt 2>&1
```

## Example Workflow:
1. Run command and redirect output to file
2. Use Read tool to examine the file
3. Extract specific information needed
4. Continue with targeted approach

This prevents:
- Truncated outputs
- Overwhelming the conversation
- Loss of important information
- Need to repeat expensive operations