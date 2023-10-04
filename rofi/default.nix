{ config, pkgs, lib, options, ... }:

if pkgs.stdenv.isLinux
then {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = with pkgs; [
        rofi-calc
        rofi-emoji
      ];
    };
    extraConfig = {
      modi = "run,emoji,calc";
      font = "mono 20";
    };
  };
}
else { };
