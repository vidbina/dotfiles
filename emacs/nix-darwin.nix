{ pkgs, lib, inputs, ... }:
let
  my-emacs = pkgs.emacs29-macport.pkgs.withPackages (epkgs: (
    with epkgs; [
      notmuch
      vterm
      pdf-tools
    ]
  ));
in
{
  environment.systemPackages = [
    # pkgs.emacs
  ];

  # See https://itecnote.com/tecnote/why-emacsclient-cant-find-socket-after-executing-emacs-daemon/
  services.emacs = {
    enable = true;
    package = my-emacs;
  };
}
