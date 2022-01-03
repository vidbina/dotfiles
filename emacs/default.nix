{ config, pkgs, lib, options, ... }:

let
  sources = import ../nix/sources.nix;
  emacs-overlay-src = sources."emacs-overlay";
in
{
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./.;

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

            # for Emacs
            (pkgs.makeDesktopItem {
              name = "org-protocol";
              exec = "${bundled-emacs}/bin/emacsclient --create-frame %u";
              comment = "Org Protocol";
              desktopName = "org-protocol";
              type = "Application";
              mimeType = "x-scheme-handler/org-protocol";
            })

            # for Emacs
            # https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-0.9.5.html
            # https://www.emacswiki.org/emacs/MailtoHandler
            # https://dev.spacekookie.de/kookie/nomicon/commit/9e5896496cfd5da5754018887f7ad3b256b3ad80.diff
            (pkgs.makeDesktopItem {
              name = "emacs-mu4e";
              exec = ''
                ${bundled-emacs}/bin/emacsclient --create-frame --eval "(browse-url-mail \"%u\")"
              '';
              comment = "Emacs mu4e";
              desktopName = "emacs-mu4e";
              type = "Application";
              mimeType = builtins.concatStringsSep ";" [
                # Email
                "x-scheme-handler/mailto"
                "message/rfc822"
              ];
            })
          ];
        });
    })
  ];
}
