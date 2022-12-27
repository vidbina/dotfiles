# Tangled from README.org
{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    isync
    mb2md
    neomutt
    offlineimap
    urlview
    msmtp
    notmuch
    notmuch-mutt
  ];
}
