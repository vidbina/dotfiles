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

  manual = {
    # Use `home-manager-help`
    html.enable = true;

    # Use `man home-configuration.nix`
    manpages.enable = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      xsel-copy-url = pkgs.writeScriptBin "xsel-copy-url" ''
        url=$1
        echo "$url" | ${pkgs.xsel}/bin/xsel -ib
        ${pkgs.libnotify}/bin/notify-send --category=url --urgency=low "🌍 Link Copied" "Paste to enter $url"
      '';
      vidbina-urxvt-themes =
        let
          readTheme = x:
            let
              text = builtins.readFile (./. + "/Xresources.d/themes/${x}");
            in
            pkgs.writeTextDir "share/${x}" text;
        in
        pkgs.symlinkJoin {
          name = "vidbina-urxvt-themes";
          paths = map readTheme [
            "minichrome-dark.Xresources"
            "minichrome-light.Xresources"
            "vidbina-dark.Xresources"
            "vidbina-light.Xresources"
          ];
        };
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

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (./. + "/tmux.conf");
  };

  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt-unicode;
    iso14755 = false;
    extraConfig = {
      "geometry" = "128x32";
      "perl-lib" = "${pkgs.rxvt-unicode}/lib/urxvt/perl";
      "perl-ext-common" = builtins.concatStringsSep "," [
        "default"
        "font-size"
        "url-select"
        "color-themes"
      ];
      "url-select.autocopy" = true;
      "url-select.launcher" = "${pkgs.xsel-copy-url}/bin/xsel-copy-url";
      "url-select.underline" = true;

      "color-themes.themedir" = "${pkgs.vidbina-urxvt-themes}/share";
      "color-themes.state-file" = "${config.home.homeDirectory}/.urxvt-theme";
      "color-themes.autosave" = 1;

      # See `man urxvt` for guidance on the colors
      "background" = "#000000";
      "foreground" = "#ffffff";
      "cursorColor" = "#00ff00";
      "color0" = "#000000"; # black, Black
      "color1" = "#ff0000"; # red, Red3
      "color2" = "#55ff55"; # green, Green3
      "color3" = "#ffd42a"; # yellow, Yellow3
      "color4" = "#2a7fff"; # blue, Blue3
      "color5" = "#dd55ff"; # magenta, Magenta3
      "color6" = "#00aad4"; # cyan, Cyan3
      "color7" = "#cccccc"; # white, AntiqueWhite
      "color8" = "#333333"; # bright black, Grey25
      "color9" = "#ff0066"; # bright red, Red
      "color10" = "#00ff00"; # bright green, Green
      "color11" = "#ff6600"; # bright yellow, Yellow
      "color12" = "#00b3ff"; # bright blue, Blue
      "color13" = "#ff2ad4"; # bright magenta, Magenta
      "color14" = "#00ffcc"; # bright cyan, Cyan
      "color15" = "#ffffff"; # bright white, White
    };
    fonts = [
      "xft:DejaVu Sans Mono:pixelsize=28:antialias=true"
      "xft:Fira Code:size=28:antialias=true"
      "xft:Iosevka:size=28:antialias=true"
    ];
    keybindings = {
      "C-minus" = "perl:font-size:decrease";
      "C-plus" = "perl:font-size:increase";
      "C-=" = "perl:font-size:reset";
      "M-u" = "perl:url-select:select_next";
      "M-C-n" = "perl:color-themes:next";
      "M-C-p" = "perl:color-themes:prev";
      "M-C-l" = "perl:color-themes:load-state";
      "M-C-s" = "perl:color-themes:save-state";
    };
  };

  services.gammastep = {
    enable = true;
    dawnTime = "6:00-7:45";
    duskTime = "18:35-20:15";
    temperature = {
      day = 5500;
      night = 3700;
    };
    tray = true;
  };

  services.blueman-applet.enable = true;
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
      #width = 150;
      widthtype = "pixel";
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = [ "xsel-copy-url.desktop" ];
      "x-scheme-handler/http" = [ "xsel-copy-url.desktop" ];
      "x-scheme-handler/https" = [ "xsel-copy-url.desktop" ];
      "x-scheme-handler/ftp" = [ "xsel-copy-url.desktop" ];
    };
  };

  xsession = {
    enable = true;
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 64;
    };
    initExtra = ''
      setxkbmap -option -model dell -layout us -variant intl -option lv3:caps_switch
    '';
    profileExtra = ''
      hsetroot -solid '#ff9800'
    '';
  };
}
