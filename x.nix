# Tangled from README.org
{ config, pkgs, lib, options, ... }:

{
  imports = [
    ./rofi/default.nix
  ];

  home.packages = with pkgs; [
    brightnessctl
    neofetch
    peek
    screenkey
    xdotool
    btop

    (writeScriptBin "colors" ''
      # https://askubuntu.com/questions/27314/script-to-display-all-terminal-colors

      for x in {0..8}; do
        for i in {30..37}; do
          for a in {40..47}; do
            echo -ne "\e[$x;$i;$a""m\\\e[$x;$i;$a""m\e[0;37;40m "
          done
          echo
        done
      done
      echo ""
    '')

    xsel-copy-url

    darkman
  ];

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "xsel-copy-url.desktop" ];
    "x-scheme-handler/about" = [ "xsel-copy-url.desktop" ];
    "x-scheme-handler/ftp" = [ "xsel-copy-url.desktop" ];
    "x-scheme-handler/http" = [ "xsel-copy-url.desktop" ];
    "x-scheme-handler/https" = [ "xsel-copy-url.desktop" ];
    "x-scheme-handler/unknown" = [ "xsel-copy-url.desktop" ];
  };

  nixpkgs.overlays = [
    (self: super: {
      xsel-copy-url = pkgs.buildEnv (
        let
          script = pkgs.writeScriptBin "xsel-copy-url" ''
            url=$1
            echo "$url" | ${pkgs.xsel}/bin/xsel -ib
            ${pkgs.libnotify}/bin/notify-send \
              --category=url \
              --urgency=low \
              "🌍 Link Copied" "Paste to enter $url"
          '';
        in
        {
          name = "xsel-copy-url";
          paths = [
            script

            (pkgs.makeDesktopItem {
              name = "xsel-copy-url";
              exec = "${script}/bin/xsel-copy-url %U";
              comment = "Open link by copying it into the clipboard with xsel";
              desktopName = "xsel-copy-url";
              type = "Application";
              categories = [
                "Network"
                "WebBrowser"
              ];
              mimeTypes = [
                "text/html"
                "x-scheme-handler/http"
                "x-scheme-handler/https"
                "x-scheme-handler/ftp"
              ];
            })
          ];
        }
      );

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
            "vidbina-dark.Xresources"
            "vidbina-light.Xresources"
          ];
        };
    })
  ];

  xdg.mimeApps = {
    enable = true;
  };

  xsession = {
    enable = true;
    initExtra = ''
      setxkbmap -option -model dell -layout us -variant intl -option lv3:caps_switch
    '';
    profileExtra = ''
      hsetroot -solid '#ff9800'
    '';
  };

  home.pointerCursor = {
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    size = 64;
    x11 = {
      enable = true;
      defaultCursor = "tcross";
    };
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
    scroll = {
      bar.enable = false;
    };
  };

  programs.autorandr = {
    enable = true;
  };

  services = {
    screen-locker = {
      enable = true;
      lockCmd = "/run/wrappers/bin/slock";
    };
  };
}
