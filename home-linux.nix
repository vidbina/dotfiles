{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./x.nix

    ./emacs

    ./rofi
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

  services.trayer = {
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
