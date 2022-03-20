{ lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs) stdenv;
in
{
  imports = [
    ./common.nix
    ./x.nix
  ];

  services.trayer = mkIf stdenv.isLinux {
    enable = true;
    settings = {
      align = "right";
      alpha = 0;
      edge = "top";
      #height = 24;
      tint = "0x00000000";
      transparent = true;
      width = 150;
      widthtype = "pixel";
    };
  };
}
