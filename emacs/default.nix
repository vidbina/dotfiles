# Tangled from README.org
{ config, pkgs, lib, options, ... }:

let
  baseCommand = windowName:
    builtins.concatStringsSep " " [
      "emacsclient -a emacs"
      ''-F "((name . \\\"${windowName}\\\"))"''
      "-c"
    ];
in
{
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./.;

  home.packages = with pkgs; [
    cask

    # General packages
    yamllint
    nodePackages.yaml-language-server
    my-emacs
    mu

    python312

    libgccjit

    # Linux packages
    (writeScriptBin "e" ''
      exec emacsclient -a emacs -c "$@"
    '')

    (makeDesktopItem {
      name = "emacs-org-protocol";
      exec = "${(baseCommand "emacs-org-protocol")} %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      categories = [
        "Utility"
        "Database"
        "TextTools"
        "TextEditor"
        "Office"
      ];
      mimeTypes = [
        "x-scheme-handler/org-protocol"
      ];
      terminal = false;
    })

    # https://emacs.stackexchange.com/questions/13927/how-to-set-emacs-as-the-default-file-manager
    (makeDesktopItem {
      name = "emacs-dired";
      exec = "emacs-dired %f";
      comment = "Emacs Dired";
      desktopName = "emacs-dired";
      categories = [
        "Utility"
        "FileManager"
        "FileTools"
      ];
      mimeTypes = [
        "inode/directory"
        "inode/symlink"
      ];
      terminal = false;
    })

    (writeScriptBin "emacs-dired" ''
      set -e
      target_path=$(printf '%q\n' "$@" | xargs realpath -e)
      echo "Sanitized target to: $target_path"

      exec emacsclient -a emacs -c -F "((name . \"emacs-dired\"))" -e "(vidbina-mime-handle-open-directory \"emacs-dired\" \"$target_path\")"
    '')
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };

  services.emacs = {
    # Restart using `systemctl --user restart emacs`
    enable = true;
    package = pkgs.my-emacs;

    client.enable = true;
  };

  nixpkgs.overlays = [
    # Imports before overlaying


    # Overlay custom Emacs build into pkgs
    (self: super:
      let
        emacs = (pkgs.emacs-unstable.override {
          withNativeCompilation = true;
          withSQLite3 = true;
          withGTK2 = false;
          withGTK3 = false;
          withTreeSitter = true;
        });
        bundled-emacs = emacs.pkgs.withPackages (epkgs: (
          with epkgs; [
            notmuch
            vterm
            pdf-tools
          ]
        ));
        ripgrep-for-doom-emacs = (pkgs.ripgrep.override {
          withPCRE2 = true;
        });
      in
      {
        my-emacs = (pkgs.buildEnv {
          name = "my-emacs";
          paths = [
            bundled-emacs
            pkgs.clang
            pkgs.cmake
            pkgs.coreutils
            pkgs.fd
            pkgs.multimarkdown
            pkgs.python39
            ripgrep-for-doom-emacs
          ];
        });
      })
  ];

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = [ "emacs-dired.desktop" ];
    "inode/symlink" = [ "emacs-dired.desktop" ];

    "message/rfc822" = [ "emacs-mu4e.desktop" ];
    "x-scheme-handler/mailto" = [ "emacs-mu4e.desktop" ];

    "x-scheme-handler/org-protocol" = [ "emacs-org-protocol.desktop" ];
  };
}
