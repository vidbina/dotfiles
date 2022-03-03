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
}
