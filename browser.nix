{ config, pkgs, lib, options, ... }:

{
  programs.chromium = {
    enable = true;
  };
}
