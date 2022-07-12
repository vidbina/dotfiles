# Tangled from README.org
{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./emacs/default-darwin.nix
  ];
}
