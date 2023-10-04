# Tangled from README.org
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    my-emacs
  ];

  nixpkgs.overlays = [
    (self: super: {
      my-emacs = pkgs.emacs29-macport.pkgs.withPackages (epkgs: (
        with epkgs; [
          notmuch
          vterm
          pdf-tools
        ]
      ));
    })
  ];

  # See https://itecnote.com/tecnote/why-emacsclient-cant-find-socket-after-executing-emacs-daemon/
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
}
