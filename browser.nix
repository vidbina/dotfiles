# Tangled from README.org
{ config, pkgs, lib, options, ... }:

let
  # TODO: Find a cleaner implementation, like an pkgs overlay at shell.nix?!?
  nur = import <NUR> { inherit pkgs; };
in
{
  home.packages = with pkgs; [
    google-chrome-dev
  ];

  programs.chromium = {
    enable = true;
    commandLineArgs = [
     # https://www.linuxuprising.com/2018/08/how-to-enable-hardware-accelerated.html
     "--enable-accelerated-video-decode"
     "--enable-logging=stderr"
     "--ignore-gpu-blocklist"
     "--use-gl=desktop"
     "--enable-features=VaapiVideoEncoder,VaapiVideoDecoder"
     "--disable-features=UseChromeOSDirectVideoDecoder"

     # https://github.com/NixOS/nixpkgs/issues/209101
     "--disable-gpu-vsync"
     "--disable-frame-rate-limit"

     # https://wiki.archlinux.org/title/chromium
     "--ignore-gpu-blocklist"
     "--enable-gpu-rasterization"
     "--enable-zero-copy"
    ];

    extensions = [
      {
        # Vimium
        # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
      }
      {
        # wasavi
        # https://chrome.google.com/webstore/detail/wasavi/dgogifpkoilgiofhhhodbodcfgomelhe
        # see https://github.com/philc/vimium/issues/2564
        id = "dgogifpkoilgiofhhhodbodcfgomelhe";
      }
      {
        # Darkreader
        # https://chrome.google.com/webstore/detail/dark-reader/eimadpbcbfnmbkopoojfekhnkhdbieeh
        id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
      }
    ];
  };

  programs.firefox = {
    enable = true;

    # NOTE: Extensions need firefox.profiles to be defined
    extensions =
      # https://nur.nix-community.org/repos/rycee/
      with nur.repos.rycee.firefox-addons; [
        multi-account-containers # needed by tridactyl
        tridactyl
        darkreader
      ];
  };
}
