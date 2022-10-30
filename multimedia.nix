# Tangled from README.org
{ config, pkgs, lib, options, ... }:

{
  home.packages = with pkgs; [
    guvcview
    v4l-utils
  ];
}
