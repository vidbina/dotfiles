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
    latitude = 52.5;
    longitude = 13.4;

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

  services.trayer = {
    enable = true;
    settings = {
      align = "right";
      alpha = 0;
      edge = "top";
      height = 20;
      monitor = "primary";
      tint = "0x00000000";
      transparent = true;
      width = 250;
      widthtype = "pixel";
    };
  };
}
