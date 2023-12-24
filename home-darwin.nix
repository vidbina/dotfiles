# Tangled from README.org
{ config, pkgs, lib, ... }:

{
  imports = [
    ./vim.nix
    ./emacs/home.nix
  ];

  home.stateVersion = "23.05";

  home.packages = with pkgs; [
  ];

  # NOTE: Copied from dev.nix
  # TODO: Figure out how to re-use dev.nix config for Darwin and Linux
  home.file = {
    ".config/git/ignore".source = ./git/ignore;
  };

  # NOTE: Copied from dev.nix
  # No corresponding option in nix-darwin, so we config this with hm
  programs.git = {
    enable = true;
    userName = "David Asabina";
    userEmail = "vid@bina.me";
    lfs.enable = true;
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

      # NOTE: Commenting out to test if `git config -l` changes
      # sendemail = {
      #   annotate = true;
      #   smtpServer = "msmtp";
      #   smtpServerOption = "-a vidbina";
      # };
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

  programs.vscode =
    let t = pkgs.my-vscode-extensions;
    in {
      enable = true;
      extensions = with t.vscode-marketplace; [
        bbenoist.nix
        be5invis.toml
        github.copilot
        hediet.vscode-drawio
        mkhl.direnv
        ms-azuretools.vscode-docker
        ms-python.python
        ms-vscode-remote.remote-containers
        tomoki1207.pdf
        vscode-org-mode.org-mode
        vscodevim.vim
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
        # https://www.roboleary.net/2021/11/06/vscode-you-dont-need-that-extension2.html#3-indentation-guides-colorization
        "editor.guides.bracketPairs" = true;
        "editor.guides.highlightActiveIndentation" = true;

        "workbench.colorTheme" = "Default High Contrast Light";
        "workbench.preferredDarkColorTheme" = "Default High Contrast";
        "workbench.preferredLightColorTheme" = "Default High Contrast Light";
      };
    };
}
