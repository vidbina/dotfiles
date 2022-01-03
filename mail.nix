{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    isync
    mb2md
    msmtp
    (mu.overrideAttrs (oldAttrs:
      let
        rev = "bbf55256e58aa62546e8bdade1d127d7e6a9b57e";
      in
      {
        version = "1.6.10-${rev}";
        src = fetchFromGitHub {
          owner = "djcb";
          repo = "mu";
          rev = "${rev}";
          sha256 = "sha256-ozIITQbt7U4qDzHjbfDyIogIkMRpX1VsBr9igdpNqcI=";
        };
        emacs = my-emacs;
      }))
    neomutt
    notmuch
    notmuch-mutt
    offlineimap
    urlview
  ];

  # TODO: Setup get-mail worker
}
