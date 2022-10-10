# Tangled from README.org
{ config, pkgs, lib, options, ... }:

{
  home.packages = with pkgs; [
    obs-studio
  ];
}
