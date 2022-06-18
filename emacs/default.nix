{ config, pkgs, lib, options, ... }:

let
  sources = import ../nix/sources.nix;
  emacs-overlay-src = sources."emacs-overlay";
in
{
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./.;

  home.packages = with pkgs; let
    baseCommand = windowName:
      builtins.concatStringsSep " " [
        "emacsclient -a emacs"
        ''-F "((name . \\\"${windowName}\\\"))"''
        "-c"
      ];
  in
  [
    cask

    (writeScriptBin "e" ''
      exec emacsclient -a emacs -c "$@"
    '')

   (makeDesktopItem {
      name = "emacs-org-protocol";
      exec = "${(baseCommand "emacs-org-protocol")} %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      categories = builtins.concatStringsSep ";" [
        "Utility"
        "Database"
        "TextTools"
        "TextEditor"
        "Office"
      ] + ";";
      mimeType = builtins.concatStringsSep ";" [
        "x-scheme-handler/org-protocol"
      ] + ";";
      terminal = false;
    })

    # https://www.emacswiki.org/emacs/MailtoHandler
    # https://dev.spacekookie.de/kookie/nomicon/commit/9e5896496cfd5da5754018887f7ad3b256b3ad80.diff
    (makeDesktopItem {
      name = "emacs-mu4e";
      exec = "emacs-mu4e %u";
      comment = "Emacs mu4e";
      desktopName = "emacs-mu4e";
      type = "Application";
      categories = builtins.concatStringsSep ";" [
        "Network"
        "Email"
      ] + ";";
      mimeType = builtins.concatStringsSep ";" [
        # Email
        "x-scheme-handler/mailto"
        "message/rfc822"
      ] + ";";
      terminal = false;
    })

    (writeScriptBin "emacs-mu4e" ''
      set -e
      target_path=$@
      echo "Target: $target_path"

      exec emacsclient -a emacs -c -F "((name . \"emacs-dired\"))" -e "(vidbina-mime-handle-open-message-in-mu4e \"emacs-dired\" \"$target_path\")"
    '')

    # https://emacs.stackexchange.com/questions/13927/how-to-set-emacs-as-the-default-file-manager
    (makeDesktopItem {
      name = "emacs-dired";
      exec = "emacs-dired %f";
      comment = "Emacs Dired";
      desktopName = "emacs-dired";
      categories = builtins.concatStringsSep ";" [
        "Utility"
        "FileManager"
        "FileTools"
      ] + ";";
      mimeType = builtins.concatStringsSep ";" [
        "inode/directory"
        "inode/symlink"
      ] + ";";
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
    (import emacs-overlay-src)

    (self: super: {
      my-emacs =
        let
          emacs = (pkgs.emacsGit.override {
            nativeComp = true;
            withSQLite3 = true;
            withGTK2 = false;
            withGTK3 = false;
          });
          emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
          bundled-emacs = emacsWithPackages (epkgs: (
            with epkgs; [
              notmuch
              vterm
              pdf-tools
            ]
          ) ++ (
            with epkgs.melpaStablePackages; [
            ]
          ) ++ (
            with epkgs.melpaPackages; [
            ]
          ));
          ripgrep-for-doom-emacs = (pkgs.ripgrep.override {
            withPCRE2 = true;
          });
          jupyter-for-emacs = (pkgs.python38.withPackages (ps: with ps; [
            jupyter
          ]));
        in
        (pkgs.buildEnv {
          name = "my-emacs";
          paths = [
            bundled-emacs
            pkgs.clang
            pkgs.cmake
            pkgs.coreutils
            pkgs.fd
            pkgs.multimarkdown
            jupyter-for-emacs
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
