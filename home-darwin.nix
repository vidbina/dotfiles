# Tangled from README.org
{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./emacs/default-darwin.nix
  ];

  programs.zsh.initExtra = ''
    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    fi;
  '';

  home.packages = with pkgs; [ qemu ];
}
