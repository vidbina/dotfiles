{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./doc.nix
    ./browser.nix
    ./x.nix

    ./emacs

    ./rofi
  ];

  home.packages = with pkgs; [
    (mu.overrideAttrs (oldAttrs:
      let
        rev = "bbf55256e58aa62546e8bdade1d127d7e6a9b57e";
      in
      {
        version = "1.6.10-${rev}";
        src = fetchFromGitHub {
          owner = "djcb";
          repo = "mu";
          rev = "${rev}";
          sha256 = "sha256-ozIITQbt7U4qDzHjbfDyIogIkMRpX1VsBr9igdpNqcI=";
        };
        emacs = my-emacs;
      }))
  ];

  services.blueman-applet.enable = true;

  services.gammastep = {
    enable = true;
    dawnTime = "5:00-6:00";
    duskTime = "17:35-19:00";
    latitude = 52.5;
    longitude = 13.4;
    temperature = {
      # https://www.eizo.com/library/basics/color_temperature_on_an_LCD_monitor/
      day = 6500;
      night = 2500;
    };
    tray = true;
  };

  services.network-manager-applet.enable = true;

  services.syncthing = {
    enable = true;
    tray = { enable = true; };
  };

  services.trayer = {
    enable = true;
    settings = {
      align = "right";
      alpha = 0;
      edge = "top";
      #height = 24;
      monitor = "primary";
      tint = "0x00000000";
      transparent = true;
      width = 150;
      widthtype = "pixel";
    };
  };
}
