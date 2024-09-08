# Tangled from README.org
{ config, pkgs, lib, ... }:

{
  imports = [
    ./vim.nix
  ];

  home.stateVersion = "23.05";

  home.packages = with pkgs; [
    pywal
    # Darwin packages for home-manager (i.e.: nix-darwin)
    alacritty
  ];

  # NOTE: Copied from dev.nix
  # TODO: Figure out how to re-use dev.nix config for Darwin and Linux
  home.file = {
    ".config/git/ignore".source = ./git/ignore;
  };

  # No corresponding option in nix-darwin, so we config this with hm
  programs.git = {
    enable = true;
    userName = "David Asabina";
    userEmail = "vid@bina.me";
    lfs.enable = true;
    aliases = {
      wdiff = "diff --word-diff --word-diff-regex='\\w+'";
      glog = "log --oneline --graph --all --decorate";
    };
    extraConfig = {
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

  # NOTE: Copied from common.nix
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  # NOTE: Enabling zsh also in hm in order to bring direnv bootstrap into scope
  # See https://gist.github.com/jmatsushita/5c50ef14b4b96cb24ae5268dab613050?permalink_comment_id=4205285#gistcomment-4205285
  programs.zsh.enable = true;

  home.file.".hammerspoon".source = config.lib.file.mkOutOfStoreSymlink ./hammerspoon;

  programs.vscode = {
    enable = true;
    extensions = with pkgs.my-vscode-extensions.vscode-marketplace; [
      bbenoist.nix
      be5invis.toml
      elmtooling.elm-ls-vscode # depends on hbenl.vscode-test-explorer
      github.copilot
      github.copilot-chat
      hbenl.vscode-test-explorer
      hediet.vscode-drawio
      mkhl.direnv
      ms-azuretools.vscode-docker
      ms-playwright.playwright
      ms-python.python
      ms-vscode-remote.remote-containers
      tomoki1207.pdf
      vscode-org-mode.org-mode
      vscodevim.vim
      gruntfuggly.todo-tree
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
    };
  };
  # home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
  # TODO: Fix hack of hardcoded dotfiles path
  # NOTE: This repo must be checked out to ~/Code/vidbina/dotfiles
  # A hardcoded .emacs.d source is used because mkOutOfStoreSymlink ./emacs
  # does not seem to work on macOS.
  # See https://discourse.nixos.org/t/accessing-home-manager-config-in-flakes/19864/8
  # See https://github.com/nix-community/home-manager/issues/2085#issuecomment-861427318
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/vidbina/dotfiles/emacs";
  services.syncthing = {
    enable = true;
  };
}
