{ config, pkgs, lib, options, ... }:

{
  home.packages = with pkgs; [
    neofetch
    peek
    screenkey
    xsel-copy-url

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
  ];

  nixpkgs.overlays = [
    (self: super: {
      xsel-copy-url =
        let
          xsel-copy-url = pkgs.writeScriptBin "xsel-copy-url" ''
            url=$1
            echo "$url" | ${pkgs.xsel}/bin/xsel -ib
            ${pkgs.libnotify}/bin/notify-send \
              --category=url \
              --urgency=low \
              "🌍 Link Copied" "Paste to enter $url"
          '';
        in
        pkgs.buildEnv {
          name = "xsel-copy-url";
          paths = [
            xsel-copy-url

            (pkgs.makeDesktopItem {
              name = "xsel-copy-url";
              exec = "${xsel-copy-url}/bin/xsel-copy-url %U";
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
        };

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
}
