{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./emacs/default-darwin.nix
  ];

  services.nix-daemon.enable = true;
}
