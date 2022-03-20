{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
}
