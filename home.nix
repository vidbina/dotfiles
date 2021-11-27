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
    pkgs.xsel-copy-url

    (pkgs.writeScriptBin "colors" ''
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

  manual = {
    # Use `home-manager-help`
    html.enable = true;

    # Use `man home-configuration.nix`
    manpages.enable = true;
  };

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

  programs.bat = {
    enable = true;
    config = {
      theme = "base16";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  programs.neovim = {
    enable = true;
    extraConfig = builtins.readFile (./. + "/nvim/init.vim");
    plugins = with pkgs.vimPlugins; [
      {
        plugin = vim-plug;
        optional = false;
      }
    ];
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withRuby = true;
  };

  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override {
      plugins = with pkgs; [
        rofi-calc
        rofi-emoji
      ];
    };
    extraConfig = {
      modi = "run,emoji,calc";
      font = "mono 25";
      theme =
        let
          getRofiThemePath = x:
            let
              text = builtins.readFile (./. + "/rofi/${x}");
              target = "share/${x}";
            in
            "${pkgs.writeTextDir target text}/${target}";
        in
        getRofiThemePath "vidbina.theme";
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
    dawnTime = "5:00-6:00";
    duskTime = "17:35-19:00";
    temperature = {
      # https://www.eizo.com/library/basics/color_temperature_on_an_LCD_monitor/
      day = 6500;
      night = 2500;
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
