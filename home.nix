{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "vidbina";
  home.homeDirectory = "/home/vidbina";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "xsel-copy-url";
      exec = "${pkgs.xsel-copy-url}/bin/xsel-copy-url %U";
      comment = "Open link by copying it into the clipboard with xsel";
      desktopName = "xsel-copy-url";
      type = "Application";
      categories = [ "Network;WebBrowser;" ];
      mimeType = builtins.concatStringsSep ";" [
        "text/html"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/ftp"
      ];
    })
  ];

  nixpkgs.overlays = [
    (self: super: {
      xsel-copy-url = pkgs.writeScriptBin "xsel-copy-url" ''
        url=$1
        echo "$url" | ${pkgs.xsel}/bin/xsel -ib
        ${pkgs.libnotify}/bin/notify-send --category=url --urgency=low "🌍 Link Copied" "Paste to enter $url"
      '';
    })
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };
}
