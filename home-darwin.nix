# Tangled from README.org
{ config, pkgs, lib, inputs, dotfilesPath, ... }:

{
  imports = [
    ./vim.nix
    ./emacs/hm.nix
  ];

  home.stateVersion = "23.05";

  home.packages = with pkgs; [
    # dev-packages
    gh
    gleam
    gnumake
    gnupg
    gotop
    html-tidy
    htop
    httpie
    httplab
    jq
    kakoune
    typescript-language-server
    nodejs
    pqrs
    nixpkgs-fmt
    redis
    sqlite-interactive
    tree
    tree-sitter
    vim
    yq

    pkgs.jujutsu
    pkgs.gh
    pkgs.glab
    inputs.devenv.packages.${pkgs.system}.devenv
    secretspec
    alacritty
    wezterm
    xxd
    hexyl
    shellcheck
    shfmt
    asciinema
    pkgs.exercism
    pkgs.html-tidy
    pkgs.httpie
    pkgs.httplab
    pkgs.checkmake
    #pkgs.cmakeCurses
    pkgs.gnumake
    pkgs.gleam
    pkgs.nixd
    pkgs.nixfmt
    pkgs.typescript-language-server
    pkgs.tree-sitter
    pkgs.jq
    pkgs.yq
    pkgs.jless
    pkgs.poppler-utils
    pkgs.sqlite-interactive
    pkgs.redis
    pkgs.fswatch
    pkgs.gemini-cli
    pkgs.codex
    pkgs.llama-cpp
    # home-darwin-packages
    pywal
    (rustPlatform.buildRustPackage {
      pname = "gref-dedup";
      version = "0.1.0";
      src = ./gref-dedup;
      # Run `nix build .#packages.aarch64-darwin.default 2>&1 | grep 'got:'` to compute.
      cargoHash = "sha256-MH3+teduU3ZQU9ZEMhxST8G0WOrxUqVoLdPEnYK8iws=";
    })
    ((buildNpmPackage.override { nodejs = nodejs_22; }) rec {
      pname = "openrig";
      version = "0.7.20";
      src = fetchurl {
        url = "https://registry.npmjs.org/@openrig/cli/-/cli-${version}.tgz";
        hash = "sha256-T75azuL8UBTiWit6eyphLMipqSM0YphYUcLfcJVAkGc=";
      };
      sourceRoot = "package";
      postPatch = ''
        cp ${./openrig/package-lock.json} package-lock.json
      '';
      npmDepsHash = "sha256-ZmH4EX/DrIDDRpkh/eKgPv3NnG9zpqHYIbGUh1AjGgI=";
      dontNpmBuild = true;
      # better-sqlite3 compiles its native module with node-gyp.
      nativeBuildInputs = [ python3 ];
      postInstall = ''
        cli=$out/lib/node_modules/@openrig/cli/daemon/assets
        mkdir -p $out/share/openrig/skills
        ln -s $cli/skills/rigs $out/share/openrig/skills/rigs
        ln -s $cli/plugins/openrig-core/skills/openrig-skills $out/share/openrig/skills/openrig-skills
        ln -s $cli/plugins/openrig-core/skills/refocusing $out/share/openrig/skills/refocusing
        ln -s $out/lib/node_modules/@openrig/cli/daemon/specs $out/share/openrig/specs
      '';
    })
    (writeShellApplication {
      name = "rignap";
      runtimeInputs = [ jq coreutils ];
      text = ''
        state_file="$HOME/.openrig/rignap-state.json"
        assume_yes=0

        for arg in "$@"; do
          case "$arg" in
            --yes|-y) assume_yes=1 ;;
            -h|--help)
              echo "usage: rignap [--yes]"
              echo "Snapshot and stop every running rig (kernel last), close viewers and stop the daemon."
              exit 0
              ;;
            *) echo "rignap: unknown argument: $arg" >&2; exit 2 ;;
          esac
        done

        if [ -n "''${OPENRIG_NODE_ID:-}" ]; then
          echo "rignap: this is an OpenRig seat; running here would stop the seat running it." >&2
          echo "Run rignap from a plain terminal." >&2
          exit 1
        fi

        daemon_up() {
          local status
          status=$(rig daemon status 2>/dev/null || true)
          [[ "$status" == "Daemon running on port"* ]]
        }

        if ! daemon_up; then
          echo "The OpenRig daemon isn't running: everything is already asleep."
          exit 0
        fi

        ps_json=$(rig ps --json)
        mapfile -t running < <(jq -r '.[] | select(.status == "running") | .name' <<<"$ps_json" | sort)

        others=()
        kernel_up=0
        for r in "''${running[@]}"; do
          if [ "$r" = "kernel" ]; then kernel_up=1; else others+=("$r"); fi
        done
        order=("''${others[@]}")
        if [ "$kernel_up" -eq 1 ]; then order+=("kernel"); fi

        # Record what is running, merged with any earlier nap's list.
        mkdir -p "$(dirname "$state_file")"
        previous='[]'
        if [ -f "$state_file" ]; then
          previous=$(jq -c '.rigs // []' "$state_file" 2>/dev/null || echo '[]')
        fi
        now_json=$(printf '%s\n' "''${running[@]}" | jq -R . | jq -sc 'map(select(length > 0))')
        jq -n --argjson prev "$previous" --argjson now "$now_json" \
          --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
          '{rigs: ($prev + $now | unique), savedAt: $ts}' >"$state_file.tmp"
        mv "$state_file.tmp" "$state_file"

        echo "rignap will:"
        if [ "''${#order[@]}" -eq 0 ]; then
          echo "  - stop no rigs (none are running)"
        else
          i=1
          for r in "''${order[@]}"; do
            echo "  $i. rig down $r --snapshot"
            i=$((i + 1))
          done
        fi
        echo "  - close the viewers OpenRig opened (rig terminal close --all)"
        echo "  - stop the daemon (rig daemon stop)"
        echo "Rigs to bring back with rigwake are recorded in $state_file."

        if [ "$assume_yes" -ne 1 ]; then
          printf 'Go ahead? [y/N] '
          read -r answer || answer=""
          case "$answer" in
            y|Y|yes|YES) ;;
            *) echo "Nothing stopped."; exit 1 ;;
          esac
        fi

        stopped=()
        for idx in "''${!order[@]}"; do
          r="''${order[$idx]}"
          echo "Stopping $r..."
          if rig down "$r" --snapshot --yes; then
            stopped+=("$r")
            echo "  $r: down"
          else
            echo "rignap: rig down $r failed; stopping here." >&2
            echo "Still up: ''${order[*]:$idx}" >&2
            echo "The daemon is still running. Fix $r and run rignap again." >&2
            exit 1
          fi
        done

        echo "Closing OpenRig viewers..."
        if ! rig terminal close --all; then
          echo "rignap: closing viewers failed; carrying on." >&2
        fi

        echo "Stopping the daemon..."
        if ! rig daemon stop; then
          echo "rignap: rig daemon stop failed; the daemon may still be running." >&2
          exit 1
        fi

        echo
        if [ "''${#stopped[@]}" -eq 0 ]; then
          echo "No rigs were running. Daemon stopped."
        else
          echo "Asleep: ''${stopped[*]}. Daemon stopped."
        fi
        echo "rigwake will bring back: $(jq -r '.rigs | join(" ")' "$state_file")"
        echo "Run rigwake to resume."
      '';
    })
    (writeShellApplication {
      name = "rigwake";
      runtimeInputs = [ jq coreutils ];
      text = let
        cleanPath = lib.concatStringsSep ":" [
          "/etc/profiles/per-user/${config.home.username}/bin"
          "${config.home.homeDirectory}/.nix-profile/bin"
          "/run/current-system/sw/bin"
          "/nix/var/nix/profiles/default/bin"
          "/opt/homebrew/bin"
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
        ];
      in ''
        state_file="$HOME/.openrig/rignap-state.json"
        clean_path="''${RIGWAKE_PATH:-${cleanPath}}"
        assume_yes=0

        for arg in "$@"; do
          case "$arg" in
            --yes|-y) assume_yes=1 ;;
            -h|--help)
              echo "usage: rigwake [--yes]"
              echo "Start the daemon and restore the rigs rignap put to sleep."
              exit 0
              ;;
            *) echo "rigwake: unknown argument: $arg" >&2; exit 2 ;;
          esac
        done

        if [ -n "''${OPENRIG_NODE_ID:-}" ]; then
          echo "rigwake: this is an OpenRig seat; run rigwake from a plain terminal." >&2
          exit 1
        fi

        daemon_up() {
          local status
          status=$(rig daemon status 2>/dev/null || true)
          [[ "$status" == "Daemon running on port"* ]]
        }

        running=()
        refresh_running() {
          local ps_json
          running=()
          ps_json=$(rig ps --json)
          mapfile -t running < <(jq -r '.[] | select(.status == "running") | .name' <<<"$ps_json")
        }

        is_running() {
          local r
          for r in "''${running[@]}"; do
            if [ "$r" = "$1" ]; then return 0; fi
          done
          return 1
        }

        have_state=0
        recorded=()
        if [ -f "$state_file" ]; then
          have_state=1
          mapfile -t recorded < <(jq -r '.rigs[]' "$state_file")
        fi

        daemon_was_up=0
        if daemon_up; then
          daemon_was_up=1
          refresh_running
        fi

        if [ "$have_state" -eq 0 ] && [ "$daemon_was_up" -eq 1 ]; then
          echo "The daemon is running and no nap is recorded in $state_file: nothing to do."
          exit 0
        fi

        to_start=()
        kernel_recorded=0
        for r in "''${recorded[@]}"; do
          if [ "$r" = "kernel" ]; then
            kernel_recorded=1
          elif ! is_running "$r"; then
            to_start+=("$r")
          fi
        done
        kernel_needed=0
        if { [ "$kernel_recorded" -eq 1 ] || [ "$have_state" -eq 0 ]; } && ! is_running kernel; then
          kernel_needed=1
        fi

        if [ "$daemon_was_up" -eq 1 ] && [ "''${#to_start[@]}" -eq 0 ] && [ "$kernel_needed" -eq 0 ]; then
          echo "Everything recorded is already running: ''${recorded[*]}"
          rm -f "$state_file"
          echo "Removed $state_file."
          exit 0
        fi

        echo "rigwake will:"
        if [ "$daemon_was_up" -eq 1 ]; then
          echo "  - leave the daemon as it is (already running)"
        else
          echo "  - start the daemon with a clean environment and wait for the kernel"
        fi
        if [ "$have_state" -eq 0 ]; then
          echo "  - restore the rigs that were last running (rig start --last): no state file at $state_file"
        elif [ "''${#to_start[@]}" -gt 0 ]; then
          echo "  - restore: ''${to_start[*]} (rig start --rigs)"
        fi
        echo "  - bring the kernel back with rig up kernel --existing if it isn't up by then"
        echo "  - open viewers (rig terminal open --all, and the kernel in a window)"

        if [ "$assume_yes" -ne 1 ]; then
          printf 'Go ahead? [y/N] '
          read -r answer || answer=""
          case "$answer" in
            y|Y|yes|YES) ;;
            *) echo "Nothing started."; exit 1 ;;
          esac
        fi

        if [ "$daemon_was_up" -eq 0 ]; then
          rig_bin=""
          IFS=: read -r -a path_dirs <<<"$clean_path"
          for d in "''${path_dirs[@]}"; do
            if [ -x "$d/rig" ]; then rig_bin="$d/rig"; break; fi
          done
          if [ -z "$rig_bin" ]; then
            echo "rigwake: no rig found on $clean_path" >&2
            exit 1
          fi
          echo "Starting the daemon ($rig_bin)..."
          if ! env -i \
              HOME="$HOME" \
              USER="''${USER:-$(id -un)}" \
              LOGNAME="''${LOGNAME:-$(id -un)}" \
              SHELL="''${SHELL:-/bin/zsh}" \
              TMPDIR="''${TMPDIR:-/tmp}" \
              LANG="''${LANG:-en_US.UTF-8}" \
              PATH="$clean_path" \
              "$rig_bin" daemon start --wait-for-kernel; then
            echo "rigwake: rig daemon start reported a problem; checking whether it came up." >&2
          fi
          if ! daemon_up; then
            echo "rigwake: the daemon isn't running. Check rig daemon logs." >&2
            exit 1
          fi
        fi

        refresh_running
        if [ "$have_state" -eq 0 ]; then
          echo "No state file: restoring the rigs that were last running (rig start --last)."
          rig start --last || echo "rigwake: rig start --last reported a problem." >&2
        else
          to_start=()
          for r in "''${recorded[@]}"; do
            if [ "$r" != "kernel" ] && ! is_running "$r"; then to_start+=("$r"); fi
          done
          if [ "''${#to_start[@]}" -gt 0 ]; then
            echo "Restoring: ''${to_start[*]}"
            rig start --rigs "''${to_start[@]}" || echo "rigwake: rig start reported a problem." >&2
          fi
        fi

        refresh_running
        if { [ "$kernel_recorded" -eq 1 ] || [ "$have_state" -eq 0 ]; } && ! is_running kernel; then
          echo "The kernel isn't up yet: rig up kernel --existing"
          rig up kernel --existing || echo "rigwake: rig up kernel reported a problem." >&2
        fi

        echo "Opening viewers..."
        rig terminal open --all || echo "rigwake: rig terminal open --all reported a problem." >&2
        rig terminal open saved:kernel --window || echo "rigwake: opening the kernel view reported a problem." >&2

        refresh_running
        if [ "$have_state" -eq 0 ]; then
          echo
          echo "Running: ''${running[*]}"
          exit 0
        fi
        missing=()
        for r in "''${recorded[@]}"; do
          if ! is_running "$r"; then missing+=("$r"); fi
        done
        echo
        if [ "''${#missing[@]}" -eq 0 ]; then
          rm -f "$state_file"
          echo "Awake: ''${recorded[*]}. Removed $state_file."
        else
          echo "Still down: ''${missing[*]}. Keeping $state_file; run rigwake again once they're fixed." >&2
          exit 1
        fi
      '';
    })
  ];

  home.file = {
    # Set global gitignore
    ".config/git/ignore".source = config.lib.file.mkOutOfStoreSymlink ./git/ignore;
    ".wezterm.lua".source = ./wezterm/wezterm.lua;
    ".config/ghostty/config.ghostty".source = config.lib.file.mkOutOfStoreSymlink ./ghostty/config.ghostty;
    ".config/borders/bordersrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/borders/bordersrc";
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/emacs";
    ".hammerspoon".source = config.lib.file.mkOutOfStoreSymlink ./hammerspoon;
    ".claude/skills".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/claude/skills";
    ".claude/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/claude/settings.json";
    ".claude/rules".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/claude/rules";
    # OpenCode config and plugins
    ".config/opencode/opencode.jsonc".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/opencode/opencode.jsonc";
    ".config/opencode/plugins".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/opencode/plugins";
    ".config/herdr/config.toml" = {
      source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/herdr/config.toml";
      force = true;
    };
  };
  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (./. + "/tmux.conf");
  };
  programs.vscode = {
    enable = true;
    profiles.default = {
      extensions = with pkgs.vscode-marketplace; [

      ];
      keybindings = [
        {
          "key" = "ctrl+tab";
          "command" = "workbench.action.nextEditorInGroup";
        }
        {
          "key" = "ctrl+shift+tab";
          "command" = "workbench.action.previousEditorInGroup";
        }
      ];
      userSettings = {
        "editor.cursorSurroundingLines" = 8;

        # https://code.visualstudio.com/docs/editor/extension-marketplace#_can-i-stop-vs-code-from-providing-extension-recommendations
        "extensions.ignoreRecommendations" = true;

        # https://code.visualstudio.com/docs/editor/extension-marketplace#_can-i-stop-vs-code-from-providing-extension-recommendations
        "telemetry.telemetryLevel" = "off";

        "vim.highlightedyank.enable" = true;

        "window.autoDetectColorScheme" = true;

        # Make the time-out a bit longer to allow for us to get that direnv stuff done
        "application.shellEnvironmentResolutionTimeout" = 20; # original was 10
        # https://www.roboleary.net/2021/11/06/vscode-you-dont-need-that-extension2.html#3-indentation-guides-colorization
        "editor.guides.bracketPairs" = true;
        "editor.guides.highlightActiveIndentation" = true;
        "workbench.preferredDarkColorTheme" = "Default High Contrast";
        "workbench.preferredLightColorTheme" = "Light 2026";
        "workbench.list.openMode" = "doubleClick";
        "claudeCode.preferredLocation" = "panel";
      };
    };
  };
  programs.bat = {
    enable = true;
    config = {
      theme = "base16";
    };
  };
  services.syncthing = {
    enable = true;
  };

  programs = {
    git = {
      enable = true;

      lfs.enable = true;
      signing.format = "ssh";

      settings = {
        user.name = "David Asabina";

        # NOTE: We avoid setting userEmail in order to have the git CLI fail
        # and explicitly ask for this when not set to avoid commits under
        # wrong addresses.
        # Recommendation: configure email on a per-folder basis, not globally.
        #user.email = "vid@bina.me";
        alias = {
          wdiff = "diff --word-diff --word-diff-regex='\\w+'";
          glog = "!git log --graph --all --format='%C(auto)%h %s%d' --color=always \"$@\" | gref-dedup | less -RF #";
          ready = "!git checkout main && git pull --ff-only";
          # Touch ID-free aliases for remote/agent sessions
          mcommit = "-c commit.gpgsign=false commit";
          mpush = "!git -c url.\"https://github.com/\".pushInsteadOf=\"git@github.com:\" push \"$@\" #";
          mpull = "!git -c url.\"https://github.com/\".insteadOf=\"git@github.com:\" pull \"$@\" #";
        };
        init = {
          defaultBranch = "main";
        };

        core = {
          editor = "nvim";
        };

        gpg = {
          program = "gpg2";
        };

        sendemail = {
          annotate = true;
          smtpServer = "msmtp";
          smtpServerOption = "-a vidbina";
        };
        color = {
          ui = true;
          diff = {
            meta = "yellow bold";
            frag = "magenta bold";
             old = "red";
             new = "green";
          };
          grep = {
            match = "yellow";
            filename = "blue";
            linenumber = "brightblack";
          };
          status = {
            added = "yellow";
            changed = "green";
            untracked = "brightblack";
          };
        };
      };
    };
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    # NOTE: Enabling zsh also in hm in order to bring direnv bootstrap into scope
    # See https://gist.github.com/jmatsushita/5c50ef14b4b96cb24ae5268dab613050?permalink_comment_id=4205285#gistcomment-4205285
    zsh.enable = true;
    zoxide.enable = true;
    fzf.enable = true;
  };
}
