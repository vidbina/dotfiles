{ pkgs, lib, ... }: {
  imports = [
    ./pinentry.nix
    ./wm.nix
  ];
}
