# Tangled from README.org
{ config, pkgs, lib, ... }:

{
  imports = [
    ./vim.nix
    ./emacs/hm.nix
  ];

  home.stateVersion = "23.05";

  home.packages = with pkgs; [
    # dev-packages
    checkmake
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
    shell-gpt
    sqlite-interactive
    tree
    tree-sitter
    vim
    yq

    pkgs.jujutsu
    pkgs.gh
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
    pkgs.nixfmt-rfc-style
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
    # home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
    # TODO: Fix hack of hardcoded dotfiles path
    # NOTE: This repo must be checked out to ~/Code/vidbina/dotfiles
    # A hardcoded .emacs.d source is used because mkOutOfStoreSymlink ./emacs
    # does not seem to work on macOS.
    # See https://discourse.nixos.org/t/accessing-home-manager-config-in-flakes/19864/8
    # See https://github.com/nix-community/home-manager/issues/2085#issuecomment-861427318
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/vidbina/dotfiles/emacs";
    ".hammerspoon".source = config.lib.file.mkOutOfStoreSymlink ./hammerspoon;
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
        "extensions.showRecommendationsOnlyOnDemand" = true;

        # https://code.visualstudio.com/docs/editor/extension-marketplace#_can-i-stop-vs-code-from-providing-extension-recommendations
        "telemetry.telemetryLevel" = "off";

        "vim.highlightedyank.enable" = true;

        "window.autoDetectColorScheme" = true;
        # https://www.roboleary.net/2021/11/06/vscode-you-dont-need-that-extension2.html#3-indentation-guides-colorization
        "editor.guides.bracketPairs" = true;
        "editor.guides.highlightActiveIndentation" = true;
        "workbench.colorTheme" = "Default High Contrast Light";
        "workbench.preferredDarkColorTheme" = "Default High Contrast";
        "workbench.preferredLightColorTheme" = "Default High Contrast Light";
        "workbench.list.openMode" = "doubleClick";
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
  };
}
