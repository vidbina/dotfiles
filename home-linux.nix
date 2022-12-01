# Tangled from README.org
{ lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./doc.nix
    ./browser.nix
    ./x.nix
    ./multimedia.nix

    ./emacs

    ./rofi
  ];

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    vokoscreen
    montserrat
  ];

  services.blueman-applet.enable = true;
  services.network-manager-applet.enable = true;

  services.gammastep = {
    enable = true;
    dawnTime = "5:00-6:00";
    duskTime = "17:35-19:00";
    latitude = 13.7;
    longitude = 100.5;

    temperature = {
      # https://www.eizo.com/library/basics/color_temperature_on_an_LCD_monitor/
      day = 6500;
      night = 2500;
    };

    tray = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };

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
