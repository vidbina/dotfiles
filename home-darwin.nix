{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./emacs/default-darwin.nix
  ];
}
