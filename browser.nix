{ config, pkgs, lib, options, ... }:

let
  # TODO: Find a cleaner implementation, like an pkgs overlay at shell.nix?!?
  nur = import <NUR> { inherit pkgs; };
in
{
  home.packages = with pkgs; [
    nyxt
  ];

  programs.chromium = {
    enable = true;

    extensions = [
      {
        # Metamask
        # https://chrome.google.com/webstore/detail/metamask/nkbihfbeogaeaoehlefnkodbefgpgknn
        id = "nkbihfbeogaeaoehlefnkodbefgpgknn";
      }
      {
        # Vimium
        # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
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

    extensions =
      # https://nur.nix-community.org/repos/rycee/
      with nur.repos.rycee.firefox-addons; [
        darkreader
        tridactyl
      ];
  };
}
