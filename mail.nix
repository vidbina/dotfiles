# Tangled from README.org
{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    isync
    mb2md
    msmtp
    neomutt
    notmuch
    notmuch-mutt
    offlineimap
    urlview
  ];
}
