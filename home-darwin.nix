# Tangled from README.org
{ config, pkgs, lib, dotfilesPath, ... }:

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
    nodePackages.typescript-language-server
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
    devenv
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
    pkgs.nodePackages.typescript-language-server
    pkgs.tree-sitter
    pkgs.jq
    pkgs.yq
    pkgs.sqlite-interactive
    pkgs.redis
    pkgs.claude-code
    pkgs.gemini-cli
    pkgs.codex
    pkgs.ollama
    # home-darwin-packages
    pywal
  ];

  home.file = {
    # Set global gitignore
    ".config/git/ignore".source = config.lib.file.mkOutOfStoreSymlink ./git/ignore;
    ".wezterm.lua".source = ./wezterm/wezterm.lua;
    ".config/ghostty/config.ghostty".source = config.lib.file.mkOutOfStoreSymlink ./ghostty/config.ghostty;
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/emacs";
    ".hammerspoon".source = config.lib.file.mkOutOfStoreSymlink ./hammerspoon;
    ".claude/skills".source = config.lib.file.mkOutOfStoreSymlink ./claude/skills;
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
        "workbench.preferredDarkColorTheme" = "Experimental Dark";
        "workbench.preferredLightColorTheme" = "Experimental Light";
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
          glog = "log --oneline --graph --all --decorate";
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
